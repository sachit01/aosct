/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This file implements the AbstractOdometery class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-06-01   arastogi      Created
* 2016-06-22   akushwah      Odometry Implementation
* 2016-07-01   akushwah      Updated the file to remove Lint Errors
* 2016-07-05   spandita      Updated with review comments (renamed to odoconfigResponseChannel_A & odoconfigResponseChannel_B)
* 2016-09-19   arastogi      Renamed getVelocity function to getSpeed. Returns uint16 now.
*                            Added console call.
* 2016-10-12   arastogi      removed the ifndef
*                            fixed wrong check for if dynamic config has changed
*                            removed error when no config response is received.
* 2016-10-03   arastogi      Fetch ma and driver direction correctly.
*                            Fixed reversing supervision.
* 2016-10-24   arastogi      Get reversing and roll away parameters from config.
* 2017-01-10   saprasad      Corrected init function call (initDone).
* 2017-01-12   saprasad      Registered speed and acceleration for Analyzer.
* 2017-03-24   spandita      Updated code with balise handling requirements
* 2017-04-12   skothiya      Updated for isTrainStandStill function implementation
* 2017-05-24   skothiya      Updated for slip and slide requirement implementation
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <cstdio>
#include "abstract_config.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include "abstract_odometry.hpp"
#include "abstract_tsetup.hpp"
#include "abstract_decode.hpp"
#include "abstract_position.hpp"
#include "abstract_targets.hpp"
#include "abstract_analyzer_if.hpp"
#include "abstract_mode_control.hpp"
#include "atc_math.hpp"
#include "dmi_event_codes.hpp"
#include "abstract_odometry_event_ids.hpp"
#include "atc_math.hpp"
#include "atc_util.hpp"
#include <vfw_string.h>
#include <vfw_checkpoints.h>
#ifndef __GNUG__
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
  const uint8_t currentSdpMajorVersion = 2U;
  const uint8_t currentSdpMiddleVersion = 2U;
  const uint8_t currentSdpMinorVersion = 4U;
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
    AbstractOdometry::AbstractOdometry() : ATC::ProcComponent(atpOdometryId, "Odometry", "ODO"),
      // creating different set of objects for different type of events
      configFailure(ATC::Event::createSafetyHaltEvent(atpOdometryId, ATC::CoreContainer, eventIdConfigFailure, ATC::NoEB,
        DMICom::odoConfigFail, "COD Configuration failure")),
      invalidState(ATC::Event::createSafetyHaltEvent(atpOdometryId, ATC::CoreContainer, eventIdInvalidState, ATC::NoEB,
        DMICom::odoInvalidState, "COD Invalid State failure")),
      noConfigResponse(ATC::Event::createSafetyHaltEvent(atpOdometryId, ATC::CoreContainer, eventIdNoConfigRespRecvFailure, ATC::NoEB,
        DMICom::noConfigRespRecv, "NO Config Response received from COD")),
      noMeasurementData(ATC::Event::createSafetyHaltEvent(atpOdometryId, ATC::CoreContainer, eventIdNoDataTelFailure, ATC::NoEB,
        DMICom::noDataTelFailure, "NO Measurement Data received from COD")),
      invalidMeasurementData(ATC::Event::createSBReqEvent(atpOdometryId, ATC::CoreContainer, eventIdInvalidDataTelegram, ATC::NoSB,
        DMICom::invalidDataTelegram, "Invalid Measurement Data received from COD")),
      safetyIssueCodData(ATC::Event::createSafetyHaltEvent(atpOdometryId, ATC::CoreContainer, eventIdSafetyIssueCodData, ATC::NoEB,
        DMICom::odoFailure, "COD is not providing safe service")),
      versionCODMismatch(ATC::Event::createSafetyHaltEvent(atpOdometryId, ATC::CoreContainer, eventIdVersionCODMismatch, ATC::NoEB,
        DMICom::odoVersionMismatch, "COD version mismatch")),
      exceededSafetyMargin(ATC::Event::createSafeBrakeSBEvent(atpOdometryId, ATC::CoreContainer, eventIdBaliseWindowFailure, ATC::NoSB,
        DMICom::odoBalWindowError, "BaliseWindow > SafetyMargin (cm): ", true)),
      odometerSpeedSensorFailureOccurred(ATC::Event::createLogEvent(atpOdometryId, ATC::CoreContainer, eventIdOdometerSpeedSensorFailureOccurred, 0x0U,
        "Odometer speed sensor failure", false)),
      odometerSpeedSensorFailureRecovered(ATC::Event::createLogEvent(atpOdometryId, ATC::CoreContainer, eventIdOdometerSpeedSensorFailureRecovered, 0x0U,
        "Odometer speed sensor failure recovered", false)),
      tachometer1FailureOccured(ATC::Event::createLogEvent(atpOdometryId, ATC::CoreContainer, eventIdTachometer1SensorFailureOccured, DMICom::tachometer1Failure,
        "Tachometer1 sensor failure", false)),
      tachometer2FailureOccured(ATC::Event::createLogEvent(atpOdometryId, ATC::CoreContainer, eventIdTachometer2SensorFailureOccured, DMICom::tachometer2Failure,
        "Tachometer2 sensor failure", false)),
      dopplerFailureOccured(ATC::Event::createLogEvent(atpOdometryId, ATC::CoreContainer, eventIdDopplerSensorFailureOccured, DMICom::dopplerFailure,
        "Doppler sensor Failure ", false)),
      dopplerRadarNeedsMaintenance(ATC::Event::createLogEvent(atpOdometryId, ATC::CoreContainer, eventIdDopplerSensorNeedMaintenance, DMICom::dopplerNeedsMaintenance,
        "Doppler radar sensor needs maintenance,been inactive for more than 10% of hour", false)),
      tachometer1FailureRecovered(ATC::Event::createLogEvent(atpOdometryId, ATC::CoreContainer, eventIdTachometer1SensorFailureRecovered, DMICom::tachometer1NormalOperation,
        "Tachometer1 sensor failure clear")),
      tachometer2FailureRecovered(ATC::Event::createLogEvent(atpOdometryId, ATC::CoreContainer, eventIdTachometer2SensorFailureRecovered, DMICom::tachometer2NormalOperation,
        "Tachometer2 sensor failure clear")),
      dopplerFailureRecovered(ATC::Event::createLogEvent(atpOdometryId, ATC::CoreContainer, eventIdDopplerSensorFailureRecovered, DMICom::dopplerNormalOperation,
        "Doppler sensor failure clear")),
      slipDetected(ATC::Event::createLogEvent(atpOdometryId, ATC::CoreContainer, eventIdSlipDetected, DMICom::slipDetected,
        "Slip detected")),
      slideDetected(ATC::Event::createLogEvent(atpOdometryId, ATC::CoreContainer, eventIdSlideDetected, DMICom::slideDetected,
        "Slide detected")),
      vNomDiffFailureOccured(ATC::Event::createSBReqEvent(atpOdometryId, ATC::CoreContainer, eventIdOdometerSpeedSensorDiffFailureOccurred, ATC::NoSB,
        DMICom::NominalSpeedDifferenceFailureOccured, "Odometer speed sensor nominal speed difference failure", false)),
      odometerSpeedSensorFailure(false),
      tachometer1Failure(false),
      tachometer2Failure(false),
      dopplerFailure(false),
      vMaxDiffTime(0U),
      vMinDiffTime(0U),
      vPos(0U)
    {
      if (coreOdometryInstancePtr != 0)
      {
        // Error handler
        ATC::aosHalt(__FILE__, __LINE__, "Odometry constructor already instantiated");
      }

      // Setup single instance pointer for core access
      coreOdometryInstancePtr = this;
      initDone = false;
      // Set State Machine Value initial to start state machine
      odometerConfigState = SendStaticConfig;
      odometerConfigSent = false;
      odometerConfigSentTime = 0;
      maxAccelComp = 0U;

      locoEndBaliseWindow = 0U;
      lastCarBaliseWindow = 0U;
      rawLocoEndBaliseWindow = 0U;
      rawLastCarBaliseWindow = 0U;
      dNomAccumulated = 0;
      dMaxAccumulated = 0;
      dMinAccumulated = 0;
      odoNom = 0;
      odoNomOld = 0;
      offset = 0;
      dirMultiplier = unknownDir;
      safetyMarginCrossedFlag = false;
      isSlipClear = false;
      isFreeRolling = false;
      dNomOld = 0;
      dMaxOld = 0;
      dMinOld = 0;

      // Set the initial value for train standstill conditions
      trainStandStillCounter = 0U;
      trainNotAtStandStillCounter = 0U;
      isTrainStandStillFlag = true;

      // Clear Analyser value
      odoValForAnalyser = 0;

      // Clear reported speed
      absNomSpeed = 0U;

      // Clear sdp version
      sdpMajorVersion = 0U;
      sdpMiddleVersion = 0U;
      sdpMinorVersion = 0U;

      memset(&odoDataStatus, 0, sizeof(odoDataStatus));
      memset(&odoData, 0, sizeof(odoData));
      memset(&odoDataRaw, 0, sizeof(odoDataRaw));
      memset(&odoDynamicConfig, 0, sizeof(odoDynamicConfig));
      memset(&vMax[0], 0, sizeof(vCacheMaxSize));
      memset(&vMin[0], 0, sizeof(vCacheMaxSize));

      odoData.timestamp = ATC::uint32Max; // must be different from the first received value

      lastLogCODDetails = 0;
    }

    /******************************************************************************
    * getOdometerSpeedSensorFailure
    ******************************************************************************/
    bool AbstractOdometry::getOdometerSpeedSensorFailure() const
    {
      return odometerSpeedSensorFailure;
    }

    /******************************************************************************
    * preInit
    ******************************************************************************/
    void AbstractOdometry::preInit()
    {
      /** Channel to receive configuration response from COD */
      VFW_ChannelDesc configResponseChannel = static_cast<VFW_ChannelDesc>(NULL);

      /** Channel to receive measured data from COD */
      VFW_ChannelDesc dataChannel = static_cast<VFW_ChannelDesc>(NULL);

      expectedBalisePassed = false;
      odoData.isSlipping = false;
      odoData.isSliding = false;

      if (vfwGetSide() == VFW_A_SIDE)
      {
        // Open a channel to be used when reading the Odometer Configuration Response Telegram from COD
        configResponseChannel = vfwChannelOpenRead(ATC::odoconfigResponseChannel_A, ATC::odoConfigResponseQueueSize,
          ATC::odoConfigResponseMsgSize, ATC::odoconfigResponseChannel_A);

        // Open a channel to be used when reading the Odometer Measurement Data telegram from COD
        dataChannel = vfwChannelOpenRead(ATC::odoMeasureDataTeleChannel_A, ATC::odoMeasureDataTeleQueueSize,
          ATC::odoMeasurementDataTelegramMsgSize, ATC::odoMeasureDataTeleChannel_A);

        // Open a channel to be used when writing Odometer Configuration telegrams to COD
        crossCompareConfigChannel.initChannel(
          ATC::odoConfigTelegramChannel_A,
          ATC::odoConfigTeleQueueSize,
          ATC::odoConfigTeleMsgSize,
          false);

        // Initialize the channel statistics for the component
        static_cast<void>(vfw_strlcpy(&(chstat[0].channelname[0]), ATC::odoconfigResponseChannel_A, sizeof(chstat[0].channelname)));
        static_cast<void>(vfw_strlcpy(&(chstat[0].channelType[0]), "read", sizeof(chstat[0].channelType)));
        chstat[0].numMsgCnt = 0U;
        chstat[0].numMsgBytesCnt = 0U;

        static_cast<void>(vfw_strlcpy(&(chstat[1].channelname[0]), ATC::odoMeasureDataTeleChannel_A, sizeof(chstat[1].channelname)));
        static_cast<void>(vfw_strlcpy(&(chstat[1].channelType[0]), "read", sizeof(chstat[0].channelType)));
        chstat[1].numMsgCnt = 0U;
        chstat[1].numMsgBytesCnt = 0U;

        static_cast<void>(vfw_strlcpy(&(chstat[2].channelname[0]), ATC::odoConfigTelegramChannel_A, sizeof(chstat[2].channelname)));
        static_cast<void>(vfw_strlcpy(&(chstat[2].channelType[0]), "write", sizeof(chstat[0].channelType)));
        chstat[2].numMsgCnt = 0U;
        chstat[2].numMsgBytesCnt = 0U;

      }
      else if (vfwGetSide() == VFW_B_SIDE)
      {
        // Open a channel to be used when reading the Odometer Configuration Response Telegram from COD
        configResponseChannel = vfwChannelOpenRead(ATC::odoconfigResponseChannel_B, ATC::odoConfigResponseQueueSize,
          ATC::odoConfigResponseMsgSize, ATC::odoconfigResponseChannel_B);

        // Open a channel to be used when reading the Odometer Measurement Data telegram from COD
        dataChannel = vfwChannelOpenRead(ATC::odoMeasureDataTeleChannel_B, ATC::odoMeasureDataTeleQueueSize,
          ATC::odoMeasurementDataTelegramMsgSize, ATC::odoMeasureDataTeleChannel_B);

        // Open a channel to be used when writing Odometer Configuration telegrams to COD
        crossCompareConfigChannel.initChannel(
          ATC::odoConfigTelegramChannel_B,
          ATC::odoConfigTeleQueueSize,
          ATC::odoConfigTeleMsgSize,
          false);

        static_cast<void>(vfw_strlcpy(&(chstat[0].channelname[0]), ATC::odoconfigResponseChannel_B, sizeof(chstat[0].channelname)));
        static_cast<void>(vfw_strlcpy(&(chstat[0].channelType[0]), "read", sizeof(chstat[0].channelType)));
        chstat[0].numMsgCnt = 0U;
        chstat[0].numMsgBytesCnt = 0U;

        static_cast<void>(vfw_strlcpy(&(chstat[1].channelname[0]), ATC::odoMeasureDataTeleChannel_B, sizeof(chstat[1].channelname)));
        static_cast<void>(vfw_strlcpy(&(chstat[1].channelType[0]), "read", sizeof(chstat[0].channelType)));
        chstat[1].numMsgCnt = 0U;
        chstat[1].numMsgBytesCnt = 0U;

        static_cast<void>(vfw_strlcpy(&(chstat[2].channelname[0]), ATC::odoConfigTelegramChannel_B, sizeof(chstat[2].channelname)));
        static_cast<void>(vfw_strlcpy(&(chstat[2].channelType[0]), "write", sizeof(chstat[0].channelType)));
        chstat[2].numMsgCnt = 0U;
        chstat[2].numMsgBytesCnt = 0U;
      }
      else
      {
        // Unexpected side: Neither VFW_A_SIDE nor VFW_B_SIDE
        // Next if condition will take care of it.
      }

      if ((NULL != configResponseChannel) && (NULL != dataChannel))
      {
        syncConfigResponseChannel = vfwSyncAddChannel(configResponseChannel, ATC::trueVfw);
        // Deactivate the event driven callback functionality.
        vfwSyncChannelDeactivate(syncConfigResponseChannel);

        syncDataChannel = vfwSyncAddChannel(dataChannel, ATC::trueVfw);
        // Deactivate the event driven callback functionality.
        vfwSyncChannelDeactivate(syncDataChannel);
      }
      else
      {
        ATC::aosHalt(__FILE__, __LINE__, "Failed to open channels");
      }
    }

    /******************************************************************************
    * init
    ******************************************************************************/
    bool AbstractOdometry::init()
    {
      readMeasurementTelegrams();

      if (!initDone)
      {
        switch (odometerConfigState)
        {
        case SendStaticConfig:
          // Send the Static Configuration Data Telegram to COD over the configChannel
          sendStaticConfig();

          odometerConfigState = WaitForStaticConfigResponse;
          odometerConfigSentTime = vfwGetReferenceTime();

          locoEndBaliseWindow = AbstractConfig::corePtr()->getBalWindowMin();
          lastCarBaliseWindow = AbstractConfig::corePtr()->getBalWindowMin();
          break;

        case WaitForStaticConfigResponse:
        {
          ConfigResponse staticConfigResponse;
          if (readConfigResponse(staticConfigResponse))
          {
            //set the SDP version
            setSDPVersion(staticConfigResponse.sdpVerMajor, staticConfigResponse.sdpVerMid, staticConfigResponse.sdpVerMinor);

            // Check the configStatus parameter, which should be 2 for the config Response
            // configStatus = 2(Valid Static Configuration Telegram received. Waiting for Dynamic Configuration Telegram)
            if (staticConfigResponse.configStatus != validStatWaitDyn)
            {
              ATC::AbstractEventHandler::corePtr()->reportEvent(configFailure, __FILE__, __LINE__);
              writeToLog(ATC::BriefLog, "static config response=",
                static_cast<uint32_t>(staticConfigResponse.configStatus), __FILE__, __LINE__);
              odometerConfigState = ConfigFailed;
            }
            else if (staticConfigResponse.qVersion != odoInterfaceVersion)
            {
              ATC::AbstractEventHandler::corePtr()->reportEvent(versionCODMismatch, __FILE__, __LINE__);
              odometerConfigState = ConfigFailed;
            }
            else if (!validateSdpVersion(sdpMajorVersion, sdpMiddleVersion, sdpMinorVersion))
            {
              ATC::AbstractEventHandler::corePtr()->reportEvent(versionCODMismatch, __FILE__, __LINE__);

              char_t buffer[100U];
              const int32_t res = snprintf(&buffer[0], sizeof(buffer), "SDP version mismatch: %d.%d.%d",
                sdpMajorVersion, sdpMiddleVersion, sdpMinorVersion);

              if ((res > 0) && (static_cast<size_t>(res) < sizeof(buffer)))
              {
                writeToLog(ATC::BriefLog, &buffer[0], __FILE__, __LINE__);
              }

              odometerConfigState = ConfigFailed;
            }
            else
            {
              // All good
              odometerConfigState = SendDynamicConfig;
            }
          }
          else
          {
            // Check if the response timed out
            int64_t timeNow = vfwGetReferenceTime();
            if ((timeNow - odometerConfigSentTime) >= odometerConfigMaxTime)
            {
              ATC::AbstractEventHandler::corePtr()->reportEvent(noConfigResponse, __FILE__, __LINE__);
              odometerConfigState = ConfigFailed;
            }
          }
          break;
        }

        case SendDynamicConfig:
          odoDynamicConfig.gradCurrent = 255U;
          odoDynamicConfig.gradDir = 0U;
          odoDynamicConfig.gradType = 255U;

          // Send the dynamic Configuration Data Telegram to COD over the configChannel
          sendDynamicConfig();

          odometerConfigState = WaitForDynamicConfigResponse;
          odometerConfigSentTime = vfwGetReferenceTime();
          break;

        case WaitForDynamicConfigResponse:
        {
          ConfigResponse dynamicConfigResponse;
          if (readConfigResponse(dynamicConfigResponse))
          {
            // Check the configStatus parameter, which should be 4 for the config Response
            // configStatus = 4 (Configuration OK)
            if (dynamicConfigResponse.configStatus != configOK)
            {
              ATC::AbstractEventHandler::corePtr()->reportEvent(configFailure, __FILE__, __LINE__);
              writeToLog(ATC::BriefLog, "dynamic config response=",
                static_cast<uint32_t>(dynamicConfigResponse.configStatus), __FILE__, __LINE__);
              odometerConfigState = ConfigFailed;
            }
            else if (dynamicConfigResponse.qVersion != odoInterfaceVersion)
            {
              ATC::AbstractEventHandler::corePtr()->reportEvent(versionCODMismatch, __FILE__, __LINE__);
              odometerConfigState = ConfigFailed;
            }
            else
            {
              // All good

              odoDynamicConfig.gradCurrent = 0U;
              odoDynamicConfig.gradDir = 0U;
              odoDynamicConfig.gradType = 1U; // gradType is calculated as average of tracks

              odometerConfigState = ConfigComplete;
            }
          }
          else
          {
            // Check if the response timed out
            int64_t timeNow = vfwGetReferenceTime();
            if ((timeNow - odometerConfigSentTime) >= odometerConfigMaxTime)
            {
              ATC::AbstractEventHandler::corePtr()->reportEvent(noConfigResponse, __FILE__, __LINE__);
              odometerConfigState = ConfigFailed;
            }
          }
          break;
        }

        case ConfigComplete:
        {
          initCrossCompare();
          // Initialization done
          initDone = true;

          ATC::AbstractAnalyzerIF* const aif = ATC::AbstractAnalyzerIF::corePtr();

          // Register measurement data to AIF component
          bool resCurSpeedAIF = aif->registerMeasurement("speed", "current vehicle speed", "cm/s", 0U, ATC::uint16Max, &absNomSpeed);

          // (Although speed is registered as uint16_t, the max value is int16Max as it is the absolute value of a signed int16_t)
          bool resCurAccAIF = aif->registerMeasurement("acceleration", "current vehicle acceleration", "cm/s^2", ATC::int16Min,
            ATC::int16Max, &(odoData.acceleration));
          bool resCurOdoAIF = aif->registerMeasurement("odometer", "current odometer value", "cm", ATC::int32Min,
            ATC::int32Max, &odoValForAnalyser);
          bool resFrontBwRaw = aif->registerMeasurement("rawLocoEndBaliseWindow", "raw positive balise window", "cm", 0U,
            ATC::uint32Max, &rawLocoEndBaliseWindow);
          bool resRearBwRaw = aif->registerMeasurement("rawLastCarBaliseWindow", "raw negative balise window", "cm", 0U,
            ATC::uint32Max, &rawLastCarBaliseWindow);
          bool resFrontBw = aif->registerMeasurement("locoEndBaliseWindow", "positive balise window", "cm", 0U,
            ATC::uint32Max, &locoEndBaliseWindow);
          bool resRearBw = aif->registerMeasurement("lastCarBaliseWindow", "negative balise window", "cm", 0U,
            ATC::uint32Max, &lastCarBaliseWindow);
          bool resSlipAIF = aif->registerMeasurement("slip", "slip",
            &odoData.isSlipping);
          bool resSlideAIF = aif->registerMeasurement("slide", "slide",
            &odoData.isSliding);

          bool resdNom = aif->registerMeasurement("dNom", "dNom", "cm", ATC::int32Min,
            ATC::int32Max, &this->odoData.dNom);
          bool resdMin = aif->registerMeasurement("dMin", "dMin", "cm", ATC::int32Min,
            ATC::int32Max, &this->odoData.dMin);
          bool resdMax = aif->registerMeasurement("dMax", "dMax", "cm", ATC::int32Min,
            ATC::int32Max, &this->odoData.dMax);
          bool travDistNom = aif->registerMeasurement("dNomAccumulated", "dNomAccumulated", "cm", ATC::int32Min,
            ATC::int32Max, &dNomAccumulated);
          bool travDistMax = aif->registerMeasurement("dMaxAccumulated", "dMaxAccumulated", "cm", ATC::int32Min,
            ATC::int32Max, &dMaxAccumulated);
          bool travDistMin = aif->registerMeasurement("dMinAccumulated", "dMinAccumulated", "cm", ATC::int32Min,
            ATC::int32Max, &dMinAccumulated);

          bool resvMax = aif->registerMeasurement("vMax", "vMax", "cm/s", ATC::int16Min,
            ATC::int16Max, &odoData.vMax);
          bool resvMin = aif->registerMeasurement("vMin", "vMin", "cm/s", ATC::int16Min,
            ATC::int16Max, &odoData.vMin);

          bool resRawdDoppler = aif->registerMeasurement("dDopplerRaw", "dDopplerRaw", "cm", ATC::int32Min,
            ATC::int32Max, &this->odoDataRaw.dDoppl);
          bool resRawdTacho1 = aif->registerMeasurement("dTacho1Raw", "dTacho1Raw", "cm", ATC::int32Min,
            ATC::int32Max, &this->odoDataRaw.dTacho1);
          bool resRawdTacho2 = aif->registerMeasurement("dTacho2Raw", "dTacho2Raw", "cm", ATC::int32Min,
            ATC::int32Max, &this->odoDataRaw.dTacho2);
          bool resRawvDoppler = aif->registerMeasurement("vDopplerRaw", "vDopplerRaw", "cm/s", ATC::int16Min,
            ATC::int16Max, &this->odoDataRaw.vDoppl);
          bool resRawvTacho1 = aif->registerMeasurement("vTacho1Raw", "vTacho1Raw", "cm/s", ATC::int16Min,
            ATC::int16Max, &this->odoDataRaw.vTacho1);
          bool resRawvTacho2 = aif->registerMeasurement("vTacho2Raw", "vTacho2Raw", "cm/s", ATC::int16Min,
            ATC::int16Max, &this->odoDataRaw.vTacho2);
          bool resRawSlipSide1 = aif->registerMeasurement("SlipSlide1Raw", "SlipSlide1Raw", "byte", 0U,
            255U, &this->odoDataRaw.slipSlideStatus1);
          bool resRawSlipSide2 = aif->registerMeasurement("SlipSlide2Raw", "SlipSlide2Raw", "byte", 0U,
            255U, &this->odoDataRaw.slipSlideStatus2);

          if (!(resCurSpeedAIF && resCurAccAIF && resCurOdoAIF && resSlideAIF && resSlipAIF && resFrontBwRaw && resRearBwRaw && resFrontBw && resRearBw &&
                resdNom && resdMin && resdMax && travDistNom && travDistMax && travDistMin && resvMax && resvMin && resRawdDoppler &&
                resRawdTacho1 && resRawdTacho2 && resRawvDoppler && resRawvTacho1 && resRawvTacho2 && resRawSlipSide1 && resRawSlipSide2))
          {
            writeToLog(ATC::BriefLog, "Register measurement failed for analyzer", __FILE__, __LINE__);
          }
          break;
        }

        case ConfigFailed:
          initDone = true;
          break;

        default:
          // Error handling
          ATC::AbstractEventHandler::corePtr()->reportEvent(invalidState, __FILE__, __LINE__);
          break;
        }

      }
      return initDone;
    }

    /******************************************************************************
    * run
    ******************************************************************************/
    void AbstractOdometry::run()
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "ODO_run");

      readMeasurementTelegrams();

      if (odometerConfigState == ConfigComplete)
      {
        // Reseting safetyMarginCrossedFlag
        safetyMarginCrossedFlag = false;

        // Reseting slip status changed
        isSlipClear = false;

        //Update the dir multiplier if any train setup received
        const DS::TrainSetup* const pTrainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();

        if (pTrainSetup != static_cast<const DS::TrainSetup*>(NULL))
        {
          if ((pTrainSetup->orientation & trainLocoOrientation) > 0U)
          {
            dirMultiplier = negDir;
          }
          else
          {
            dirMultiplier = posDir;
          }
        }

        if (dirMultiplier != unknownDir)
        {
          // Get Offset value and Is balise encountered from position Component
          offset = offset + AbstractPosition::corePtr()->getOdometerOffsetCorrection();

          processMeasurementData();

          updateBaliseWindow();

          processSlipSlideCheck();

          updateDynamicConfig();
        }
        else
        {
          const TravelDir dir = Kernel::AbstractModeControl::corePtr()->getCurrentDrivingDirection();
          if (DirForward == dir)
          {
            dirMultiplier = posDir;
          }
          else if (DirReverse == dir)
          {
            dirMultiplier = negDir;
          }
          else
          {
            // do nothing
          }
        }

        odoValForAnalyser = getOdoPosition();

        checkTrainStandStill();

        logCODDetails(false);
      }
    }

    /******************************************************************************
    * sendStaticConfig
    ******************************************************************************/
    void AbstractOdometry::sendStaticConfig()
    {
      uint16_t noOfPulse1 = AbstractConfig::corePtr()->getTachoPulsesPer10Revolutions1();
      uint16_t noOfPulse2 = AbstractConfig::corePtr()->getTachoPulsesPer10Revolutions2();
      const uint16_t wheelSize1 = AbstractConfig::corePtr()->getWheelSize1();
      const uint16_t wheelSize2 = AbstractConfig::corePtr()->getWheelSize2();

      uint16_t minWheelSize =  ATC::ATCMath::minimum(wheelSize1, wheelSize2);

      // Writing the Static Configuration data telegram to COD on channel "UserAppl_To_Odo_A/B"
      // To handle the WheelSize smaller than the acceptance range of COD
      if (minWheelSize < minAcceptableWheelSizebyCOD)
      {
        noOfPulse1 = static_cast<uint16_t>((noOfPulse1 * minAcceptableWheelSizebyCOD) / minWheelSize);
        noOfPulse2 = static_cast<uint16_t>((noOfPulse2 * minAcceptableWheelSizebyCOD) / minWheelSize);
      }

      crossCompareConfigChannel.putUint8(staticConfigTelType);
      crossCompareConfigChannel.putUint8(interfaceVerSupported);
      crossCompareConfigChannel.putUint8(AbstractConfig::corePtr()->getCODSensorConfiguration());
      crossCompareConfigChannel.putUint8(AbstractConfig::corePtr()->getTractionControlValue());
      // TODO:get from config Component or Can be set as 1 or 2 (1=4kHz and 2=10kHz)
      crossCompareConfigChannel.putUint8(maxPulseCounterFreq);
      // Set as 1 or 2 (1=Train traveling in Positive direction, 2= Vice Versa)
      crossCompareConfigChannel.putUint8(AbstractConfig::corePtr()->getTacho1Direction());
      // Input pulse count from tachometer, number of pulses per 10 revolutions.
      crossCompareConfigChannel.putUint16(noOfPulse1);
      // Set as 1 or 2 (1=Train traveling in Positive direction, 2= Vice Versa)
      crossCompareConfigChannel.putUint8(AbstractConfig::corePtr()->getTacho2Direction());
      // Any pre-defined Value between 200-20000 based on requirements
      crossCompareConfigChannel.putUint16(noOfPulse2);
      crossCompareConfigChannel.putUint16(AbstractConfig::corePtr()->getMinDopplerSpeed());
      crossCompareConfigChannel.putUint16(AbstractConfig::corePtr()->getMaxDopplerSpeed());
      crossCompareConfigChannel.putUint16(AbstractConfig::corePtr()->getMaxDopplerAccel());
      /*
      * getNoOfOdoCycles() returns the number of COD-cycles between each sending of telegrams to ATP. One COD-cycle is 50 ms.
      * In the simulated environment COD is simulated by CODSim.
      * CODSIm is scheduled each 100 ms so it can not send telegrams to ATP more frequent than each 100 ms
      * This means that in the simulated environment it can not be less than 2 but in the real target environment
      * we may set it to 1. For simulated environment the value is hard-coded to 2 in codsim
      */
      // channel 1
      // Number of Odometer cycles between Odometer Measurement Data Telegrams sent on VFW channel
      crossCompareConfigChannel.putUint8(noOfOdoCycles);
      // Number of Odometer cycles between FFFIS Odometer Message Telegrams sent on VFW channel
      crossCompareConfigChannel.putUint8(notUsed);
      // Number of Odometer cycles between FFFIS Odometer Parameter Message Telegrams sent on VFW channel
      crossCompareConfigChannel.putUint8(notUsed);
      // Num of Odometer cycles between FFFIS Odometer Status Message Telegrams sent on VFW channel
      crossCompareConfigChannel.putUint8(notUsed);
      // Num of Odometer cycles between PZB_LZB Measurement Data Telegrams sent on VFW channel
      crossCompareConfigChannel.putUint8(notUsed);

      // The below parameters can be set to zero as they are not being used in this project.
      // channel 2
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);
      // channel 3
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);
      // channel 4
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);
      // channel 5
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);
      // channel 6
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);
      // channel 7
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);
      // channel 8
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);
      crossCompareConfigChannel.putUint8(notUsed);

      // No tachometer is powersupplied
      crossCompareConfigChannel.putUint8(noOSUPowerSupply);
      // Tacho1 and Tacho2 are power-supplied (and supervised) from OSU
      // crossCompareConfigChannel.putUint8(osuPowerSupplyTacho1AndTacho2);

      // Update channel stats
      chstat[2].numMsgCnt++;
      chstat[2].numMsgBytesCnt += sizeStaticConfigTelegram;
    }

    /******************************************************************************
    * readConfigResponse
    ******************************************************************************/
    bool AbstractOdometry::readConfigResponse(ConfigResponse &configResponseValue)
    {
      bool returnValue = false;
      int32_t noOfBytesRead = 0;
      // If there is any message available on configResponseChannel
      if (vfwSyncChannelStat(syncConfigResponseChannel) > 0U)
      {
        uint8_t configResponseTelegram[ATC::odoConfigResponseMsgSize];
        // Read one message
        VFW_ChannelCheck check = { VFW_ChannelErrorNone, 0U };
        noOfBytesRead = vfwSyncChannelReadCheck(
          syncConfigResponseChannel, &configResponseTelegram[0], ATC::odoConfigResponseMsgSize, &check);

        if (check.error != VFW_ChannelErrorNone)
        {
          writeToLog(ATC::BriefLog, "COD channel error:", static_cast<uint32_t>(check.error), __FILE__, __LINE__);
          ATC::aosHalt(__FILE__, __LINE__, "COD channel error");
        }
        if (check.timeSinceProduced > ATC::maxChannelTransmissionTime)
        {
          writeToLog(ATC::BriefLog, "COD message too old:", check.timeSinceProduced, __FILE__, __LINE__);
          ATC::aosHalt(__FILE__, __LINE__, "COD message too old");
        }

        // Update channel stats
        chstat[0].numMsgCnt++;
        chstat[0].numMsgBytesCnt += static_cast<uint32_t>(noOfBytesRead);

        VFW_Buffer messageBuffer;
        vfwInitBuffer(&messageBuffer, &configResponseTelegram[0], ATC::odoConfigResponseMsgSize);
        vfwSetReadBuffer(&messageBuffer, ATC::odoConfigResponseMsgSize);
        configResponseValue.telegramType = vfwGetU8(&messageBuffer);
        configResponseValue.qVersion = vfwGetU8(&messageBuffer);
        configResponseValue.sdpVerMajor = vfwGetU8(&messageBuffer);
        configResponseValue.sdpVerMid = vfwGetU8(&messageBuffer);
        configResponseValue.sdpVerMinor = vfwGetU8(&messageBuffer);
        configResponseValue.configStatus = vfwGetU8(&messageBuffer);
        configResponseValue.calStatus = vfwGetU8(&messageBuffer);
        configResponseValue.tDvTrain = vfwGetU32(&messageBuffer);
        configResponseValue.calTachoDistance1 = vfwGetU32(&messageBuffer);
        configResponseValue.tachoCalResultStatus1 = vfwGetU8(&messageBuffer);
        configResponseValue.calTachoDistance2 = vfwGetU32(&messageBuffer);
        configResponseValue.tachoCalResultStatus2 = vfwGetU8(&messageBuffer);
        configResponseValue.calDopplerDistance = vfwGetU32(&messageBuffer);
        configResponseValue.dopplerCalResultStatus = vfwGetU8(&messageBuffer);

        if ((noOfBytesRead > 0) && (configResponseValue.telegramType == configResTelType))
        {
          returnValue = true;
        }
      }

      return returnValue;
    }

    /******************************************************************************
    * sendDynamicConfig
    ******************************************************************************/
    void AbstractOdometry::sendDynamicConfig()
    {
      uint16_t wheelSize1 = AbstractConfig::corePtr()->getWheelSize1();
      uint16_t wheelSize2 = AbstractConfig::corePtr()->getWheelSize2();
      const uint16_t minWheelSize = ATC::ATCMath::minimum(wheelSize1, wheelSize2);

      //When the configured WheelSize is less than the acceptance range of COD
      if (minWheelSize < minAcceptableWheelSizebyCOD)
      {
        wheelSize1 = static_cast<uint16_t>((minAcceptableWheelSizebyCOD * wheelSize1) / minWheelSize);
        wheelSize2 = static_cast<uint16_t>((minAcceptableWheelSizebyCOD * wheelSize2) / minWheelSize);
      }

      crossCompareConfigChannel.putUint8(dynConfigTelType);
      crossCompareConfigChannel.putUint8(interfaceVerSupported);
      crossCompareConfigChannel.putUint16(AbstractConfig::corePtr()->getMaxAcceleration());
      crossCompareConfigChannel.putUint16(AbstractConfig::corePtr()->getMaxDeceleration());
      crossCompareConfigChannel.putUint8(odoDynamicConfig.gradCurrent);
      crossCompareConfigChannel.putUint8(odoDynamicConfig.gradDir);
      crossCompareConfigChannel.putUint8(odoDynamicConfig.gradType);
      crossCompareConfigChannel.putUint16(wheelSize1);
      crossCompareConfigChannel.putUint8(AbstractConfig::corePtr()->getMaxWheelSizeError());
      crossCompareConfigChannel.putUint16(wheelSize2);
      crossCompareConfigChannel.putUint8(AbstractConfig::corePtr()->getMaxWheelSizeError()); 
      crossCompareConfigChannel.putUint32(AbstractConfig::corePtr()->getDopplerPulsesPerKm());
      crossCompareConfigChannel.putUint8(AbstractConfig::corePtr()->getDopplerPulsePrecision());
      crossCompareConfigChannel.putUint8(1U); // CAL_FLAG == 1, , Dynamic sensor calibration not used
      crossCompareConfigChannel.putUint64(static_cast<uint64_t>(notUsed));
      crossCompareConfigChannel.putUint8(brakeLevNotSupplied);
      crossCompareConfigChannel.putUint8(255U); // Value not provided by User Application

      // Update the compensated max acceleration (which is sent to OPC)
      maxAccelComp = AbstractConfig::corePtr()->getMaxAcceleration();
      if (odoDynamicConfig.gradCurrent != 255U)
      {
        if (odoDynamicConfig.gradDir == gradDownhillDir)
        {
          maxAccelComp += odoDynamicConfig.gradCurrent;
        }
        else if (maxAccelComp >= odoDynamicConfig.gradCurrent)
        {
          maxAccelComp -= odoDynamicConfig.gradCurrent;
        }
        else
        {
          maxAccelComp = 0U;
        }
      }

      // Update channel stats
      chstat[2].numMsgCnt++;
      chstat[2].numMsgBytesCnt += sizeDynamicConfigTelegram;

      writeToLog(ATC::DetailedLog, "Num of Msgs on OdoChannel:", chstat[2].numMsgCnt, __FILE__, __LINE__);
      writeToLog(ATC::DetailedLog, "numMsgBytesCnt on OdoChannel:", chstat[2].numMsgBytesCnt, __FILE__, __LINE__);
    }

    
    /******************************************************************************
    * validateSdpVersion
    ******************************************************************************/
    bool AbstractOdometry::validateSdpVersion(const uint8_t sdpMajorVer, const uint8_t sdpMiddleVer, const uint8_t sdpMinorVer) const
    {
      return (sdpMajorVer == currentSdpMajorVersion) && (sdpMiddleVer == currentSdpMiddleVersion) && (sdpMinorVer >= currentSdpMinorVersion);
    }

    /******************************************************************************
    * isConfigChanged
    ******************************************************************************/
    bool AbstractOdometry::OdoDynamicConfig::isConfigChanged()
    {
      bool returnValue = false;
      int32_t gradCurrentread = DS::AbstractTargets::corePtr()->getCurGradient();

      if (gradCurrentread != gradPrev)
       {
          // Return TRUE to send that the Config is changed
          returnValue = true;
          gradCurrent = static_cast<uint8_t>(ATC::ATCMath::instance().absolute(gradCurrentread, __FILE__, __LINE__));
          // Check the gradient direction
          if (gradCurrentread < 0)
          {
            gradDir = gradDownhillDir;
          }
          else
          {
            gradDir = gradUphillDir;
          }
          gradPrev = gradCurrentread;
        }
      return returnValue;
    }

    /******************************************************************************
    * updateDynamicConfig
    ******************************************************************************/
    void AbstractOdometry::updateDynamicConfig()
    {
      if (!odometerConfigSent)
      {
        // Check if there is a change in the configuration
        if (odoDynamicConfig.isConfigChanged())
        {
          sendDynamicConfig();
          odometerConfigSent = true;
          odometerConfigSentTime = vfwGetReferenceTime();
        }
      }
      else
      {
        ConfigResponse configResponse;
        bool configStatus = readConfigResponse(configResponse);
        if (configStatus)
        {
          odometerConfigSent = false;
        }
        else
        {
          // Check if the response timed out
          int64_t timeNow = vfwGetReferenceTime();
          if ((timeNow - odometerConfigSentTime) >= odometerConfigMaxTime)
          {
            ATC::AbstractEventHandler::corePtr()->reportEvent(noConfigResponse, __FILE__, __LINE__);
          }
        }
      }
    }

    /******************************************************************************
    * readMeasurementTelegrams
    ******************************************************************************/
    void AbstractOdometry::readMeasurementTelegrams()
    {
      uint8_t rawBuffer[ATC::odoMeasurementDataTelegramMsgSize];
      VFW_Buffer messageBuffer;
      vfwInitBuffer(&messageBuffer, &rawBuffer[0], sizeof(rawBuffer));

      // Store the previous odometer values
      dNomOld = odoData.dNom;
      dMaxOld = odoData.dMax;
      dMinOld = odoData.dMin;
      odoNomOld = odoNom;

      bool odoMessageReceived = false;

      // Read as long there is any message available on dataChannel (only last message will be used)
      while (vfwSyncChannelStat(syncDataChannel) > 0U)
      {
        // Read one message
        VFW_ChannelCheck check = { VFW_ChannelErrorNone, 0U };
        const int32_t noOfBytesRead = vfwSyncChannelReadCheck(
          syncDataChannel, &rawBuffer[0], sizeof(rawBuffer), &check);

        // Ignore all data until init is done
        if (initDone)
        {
          if (noOfBytesRead > 0)
          {
            Support::AbstractCrossCompare::corePtr()->addCrossCompareInputData(&rawBuffer[0], static_cast<uint16_t>(noOfBytesRead));
            vfwSetReadBuffer(&messageBuffer, static_cast<uint32_t>(noOfBytesRead));
          }
          else
          {
            vfwSetReadBuffer(&messageBuffer, 0U);
          }

          // Update channel statistics
          chstat[1].numMsgCnt++;
          chstat[1].numMsgBytesCnt += vfwGetValidSize(&messageBuffer);

          if (check.error != VFW_ChannelErrorNone)
          {
            char_t  buffer[512];
            const int32_t ret = snprintf(&buffer[0], sizeof(buffer), "COD channel error: %d", static_cast<int32_t>(check.error));

            if ((ret > 0) && (static_cast<size_t>(ret)  < sizeof(buffer)))
            {
              ATC::aosHalt(__FILE__, __LINE__, &buffer[0]);
            }
            else
            {
              ATC::aosHalt(__FILE__, __LINE__, "COD channel error");
            }
          }
          else if (check.timeSinceProduced > ATC::maxChannelTransmissionTime)
          {
            char_t  buffer[512];
            const int32_t ret = snprintf(&buffer[0], sizeof(buffer), "COD message too old: %u", check.timeSinceProduced);

            if ((ret > 0) && (static_cast<size_t>(ret)  < sizeof(buffer)))
            {
              ATC::aosHalt(__FILE__, __LINE__, &buffer[0]);
            }
            else
            {
              ATC::aosHalt(__FILE__, __LINE__, "COD message too old");
            }
          }
          else if (readMeasurementTelegram(messageBuffer))
          {
            odoMessageReceived = true;
          }
          else
          {
            ATC::AbstractEventHandler::corePtr()->reportEvent(invalidMeasurementData, __FILE__, __LINE__);
          }
        }
      } // while

      if ((!odoMessageReceived)  &&  initDone)
      {
        // Init done, but no message this cycle, check config packet is received
        const int64_t timeNow = vfwGetReferenceTime();
        if ((timeNow - odometerConfigSentTime) >= odometerConfigMaxTime)
        {
          ATC::AbstractEventHandler::corePtr()->reportEvent(noMeasurementData, __FILE__, __LINE__);
        }
      }
    }

    /******************************************************************************
    * readMeasurementTelegram
    ******************************************************************************/
    bool AbstractOdometry::readMeasurementTelegram(VFW_Buffer& messageBuffer)
    {
      bool success = false;

      if (vfwGetValidSize(&messageBuffer) == ATC::odoMeasurementDataTelegramMsgSize)
      {
        // See Odometer Measurement Data Telegram in 3NSS012264D0033
        const uint8_t telegramType = vfwGetU8(&messageBuffer);     // TELEGRAM_TYPE
        const uint8_t qVersion = vfwGetU8(&messageBuffer);         // Q_VERSION
        const uint8_t qOdoSafe = vfwGetU8(&messageBuffer);         // Q_ODOSAFE
        const uint16_t qControl = vfwGetU16(&messageBuffer);       // Q_CONTROL
        const uint8_t qDirErr = vfwGetU8(&messageBuffer);          // Q_DIR_ERR
        const uint32_t tDvTrain = vfwGetU32(&messageBuffer);       // T_DV_TRAIN
        const uint32_t tProdTime = vfwGetU32(&messageBuffer);      // PROD_TIME
        const int16_t aTrain = vfwGetI16(&messageBuffer);          // A_TRAIN

        const int16_t vMaxCurrent = vfwGetI16(&messageBuffer);     // V_MAX
        const int16_t vNomCurrent = vfwGetI16(&messageBuffer);     // V_NOM
        const int16_t vMinCurrent = vfwGetI16(&messageBuffer);     // V_MIN

        const int32_t dMax = vfwGetI32(&messageBuffer);            // D_MAX
        const int32_t dNom = vfwGetI32(&messageBuffer);            // D_NOM
        const int32_t dMin = vfwGetI32(&messageBuffer);            // D_MIN
        const uint8_t tRadarPlausible = vfwGetU8(&messageBuffer);  // T_RADAR_PLAUSIBLE
        const uint8_t slipSlideStatus1 = vfwGetU8(&messageBuffer); // SLIP_SLIDE_STATUS_1
        const uint8_t slipSlideStatus2 = vfwGetU8(&messageBuffer); // SLIP_SLIDE_STATUS_2

        const int16_t vTacho1 = vfwGetI16(&messageBuffer);  // V_TACHO (1)
        const int16_t vTacho2 = vfwGetI16(&messageBuffer);  // V_TACHO (2)
        const int16_t vDoppl = vfwGetI16(&messageBuffer);  // V_DOPPLER
        const int32_t dTacho1 = vfwGetI32(&messageBuffer);  // D_TACHO1
        const int32_t dTacho2 = vfwGetI32(&messageBuffer);  // D_TACHO2
        const int32_t dDoppl = vfwGetI32(&messageBuffer);  // D_DOPPLER

        writeToLog(ATC::VeryDetailedLog, "slip slide 1 = ", static_cast<uint32_t>(slipSlideStatus1), __FILE__, __LINE__);
        writeToLog(ATC::VeryDetailedLog, "slip slide 2 = ", static_cast<uint32_t>(slipSlideStatus2), __FILE__, __LINE__);
        writeToLog(ATC::VeryDetailedLog, "vTacho1 = ", vTacho1, __FILE__, __LINE__);
        writeToLog(ATC::VeryDetailedLog, "vTacho2 = ", vTacho2, __FILE__, __LINE__);
        writeToLog(ATC::VeryDetailedLog, "vDoppl = ", vDoppl, __FILE__, __LINE__);
        writeToLog(ATC::VeryDetailedLog, "dTacho1 = ", dTacho1, __FILE__, __LINE__);
        writeToLog(ATC::VeryDetailedLog, "dTacho2 = ", dTacho2, __FILE__, __LINE__);
        writeToLog(ATC::VeryDetailedLog, "dDoppl = ", dDoppl, __FILE__, __LINE__);

        // Save for console print-out
        odoDataStatus.qControl = qControl;
        odoDataStatus.qOdoSafe = qOdoSafe;

        // Mark raw data as failing unless the below checks pass
        odoDataRaw.lastValid = false;

        const uint32_t bytesLeftInBuffer = vfwGetValidSize(&messageBuffer);
        if (bytesLeftInBuffer != 0U)
        {
          writeToLog(ATC::BriefLog, "Buffer not emptied correctly, bytes left: ", bytesLeftInBuffer, __FILE__, __LINE__);
        }
        else if (telegramType != odoMeasDataTelType)
        {
          writeToLog(ATC::BriefLog, "TELEGRAM_TYPE not correct: ", static_cast<uint32_t>(telegramType), __FILE__, __LINE__);
        }
        else if (qVersion != odoInterfaceVersion)
        {
          ATC::AbstractEventHandler::corePtr()->reportEvent(versionCODMismatch, __FILE__, __LINE__);
        }
        else if (qOdoSafe != fullService)
        {
          ATC::AbstractEventHandler::corePtr()->reportEvent(safetyIssueCodData, __FILE__, __LINE__);
        }
        else if (tDvTrain == odoData.timestamp)
        {
          ATC::AbstractEventHandler::corePtr()->reportEvent(noMeasurementData, __FILE__, __LINE__);
        }
        else if ((qControl & odoMeasDataQControlBitsNotUsed) != 0U)
        {
          writeToLog(ATC::BriefLog, "Q_CONTROL not correct: ", static_cast<uint32_t>(qControl), __FILE__, __LINE__);
        }
        else if (qDirErr > 1U)
        {
          writeToLog(ATC::BriefLog, "Q_DIR_ERR not correct: ", static_cast<uint32_t>(qDirErr), __FILE__, __LINE__);
        }
        else if ((tRadarPlausible >= odoTRadarPlausibleIllegalMin) &&
          (tRadarPlausible <= odoTRadarPlausibleIllegalMax))
        {
          writeToLog(ATC::BriefLog, "T_RADAR_PLAUSIBLE not correct: ", static_cast<uint32_t>(tRadarPlausible), __FILE__, __LINE__);
        }
        else if ((slipSlideStatus1 & odoMeasDataSlipSlideStatusBitsNotUsed) != 0U)
        {
          writeToLog(ATC::BriefLog, "SLIP_SLIDE_STATUS_1 illegal bits(5-7) set: ", static_cast<uint32_t>(slipSlideStatus1), __FILE__, __LINE__);
        }
        else if ((slipSlideStatus2 & odoMeasDataSlipSlideStatusBitsNotUsed) != 0U)
        {
          writeToLog(ATC::BriefLog, "SLIP_SLIDE_STATUS_2 illegal bits(5-7) set:", static_cast<uint32_t>(slipSlideStatus2), __FILE__, __LINE__);
        }
        else
        {
          // All ranges OK, we can use the values...

          // Save raw data for 'odoRaw' console command
          odoDataRaw.lastValid = true;
          odoDataRaw.slipSlideStatus1 = slipSlideStatus1;
          odoDataRaw.slipSlideStatus2 = slipSlideStatus2;
          odoDataRaw.vTacho1 = vTacho1;
          odoDataRaw.vTacho2 = vTacho2;
          odoDataRaw.vDoppl = vDoppl;
          odoDataRaw.dTacho1 = dTacho1;
          odoDataRaw.dTacho2 = dTacho2;
          odoDataRaw.dDoppl = dDoppl;

          // Check if any speed sensor bit is set...
          const bool newOdometerSpeedSensorFailure = (qControl & odoMeasDataQControlSpeedSensorBitmask) != 0U;

          // Evaluate Odometer components, Tachometer and Doppler radar for publishing to other components
          evaluateOdometerSensorFailure();

          if (newOdometerSpeedSensorFailure && (!odometerSpeedSensorFailure))
          {
            // We have a new failure, and not reported before
            odometerSpeedSensorFailure = true;

            ATC::AbstractEventHandler::corePtr()->reportEvent(odometerSpeedSensorFailureOccurred, __FILE__, __LINE__);
          }
          else if ((!newOdometerSpeedSensorFailure) && odometerSpeedSensorFailure)
          {
            // The failure is not reported any longer...
            odometerSpeedSensorFailure = false;
            ATC::AbstractEventHandler::corePtr()->reportEvent(odometerSpeedSensorFailureRecovered, __FILE__, __LINE__);
          }
          else
          {
            // Same, do nothing...
          }

          odoData.qControl = qControl;
          odoData.isOdoSafe = true; // qOdoSafe == fullService - see 'if (qOdoSafe != fullService)' above
          odoData.dirError = (qDirErr == directionKnown);
          odoData.timestamp = tDvTrain;
          odoData.tProdTime = tProdTime;
          odoData.acceleration = aTrain;
         
          odoData.vNom = vNomCurrent;
          odoData.vMax = vMaxCurrent;
          odoData.vMin = vMinCurrent;

          odoData.dNom = dNom;
          odoData.dMax = dMax;
          odoData.dMin = dMin;

          vPos = ((vPos + 1U) % vCacheMaxSize);
          vMax[vPos] = vMaxCurrent;
          vMin[vPos] = vMinCurrent;

          int32_t vMaxTmp = 0;
          int32_t vMinTmp = 0;
          for (uint8_t i = 0U; i < vCacheMaxSize; i++)
          {
            vMaxTmp += vMax[i];
            vMinTmp += vMin[i];
          }
          odoData.vMaxFiltered = static_cast<int16_t>((vMaxTmp / static_cast<int8_t>(vCacheMaxSize)));
          odoData.vMinFiltered = static_cast<int16_t>((vMinTmp / static_cast<int8_t>(vCacheMaxSize)));

          // Absolute value of speed is reported as AOS speed and also available for AOS Analyzer
          absNomSpeed = static_cast<uint16_t>(ATC::ATCMath::instance().absolute(odoData.vNom, __FILE__, __LINE__));

          const uint8_t toPercentage = 100U;
          const uint32_t vNomMargin = static_cast<uint32_t>((static_cast<uint32_t>(absNomSpeed) * AbstractConfig::corePtr()->getVNomMargin()) / toPercentage);
          
          if ((absNomSpeed + vNomMargin) < static_cast<uint16_t>(ATC::ATCMath::instance().absolute(odoData.vMaxFiltered, __FILE__, __LINE__)))
          {
            vMaxDiffTime += 1U;
          }
          else
          {
            vMaxDiffTime = 0U;
          }

          if ((absNomSpeed + vNomMargin) < static_cast<uint16_t>(ATC::ATCMath::instance().absolute(odoData.vMinFiltered, __FILE__, __LINE__)))
          {
            vMinDiffTime += 1U;
          }
          else
          {
            vMinDiffTime = 0U;
          }

          if ((vMaxDiffTime >= vNomDiffTimeMax) || (vMinDiffTime >= vNomDiffTimeMax))
          {
            ATC::AbstractEventHandler::corePtr()->reportEvent(vNomDiffFailureOccured, __FILE__, __LINE__);
          }

          success = true;
        }
      }

      return success;
    }

    /******************************************************************************
    * processMeasurementData
    ******************************************************************************/
    void AbstractOdometry::processMeasurementData()
    {
      // Calculate the nominal distance traveled
      const int32_t nomIncrement = static_cast<int32_t>(dirMultiplier) * (odoData.dNom - dNomOld);

      odoNom += nomIncrement;

      // Store the current odometer direction in old odometer direction
      odoData.odoDirOld = odoData.odoDir;

      if (odoData.vNom > 0)
      {
        if (dirMultiplier == posDir) // B end facing cars
        {
          odoData.odoDir = DirForward;
        }
        else
        {
          odoData.odoDir = DirReverse;
        }
      }
      else if (odoData.vNom < 0)
      {
        if (dirMultiplier == negDir) // A end facing cars
        {
          odoData.odoDir = DirForward;
        }
        else
        {
          odoData.odoDir = DirReverse;
        }
      }
      else
      {
        odoData.odoDir = DirUndefined;
      }
    }

    /******************************************************************************
    * resetBaliseWindow
    ******************************************************************************/
    void AbstractOdometry::resetBaliseWindow()
    {
      rawLocoEndBaliseWindow = 0U;
      rawLastCarBaliseWindow = 0U;
      dNomAccumulated = 0;
      dMaxAccumulated = 0;
      dMinAccumulated = 0;
    }

    /******************************************************************************
    * updateBaliseWindow
    ******************************************************************************/
    void AbstractOdometry::updateBaliseWindow()
    {
      static uint32_t beginCp = 0U; // Must be initialized to 0
      static uint32_t endCp = 0U; // Must be initialized to 0

      AbstractDecode::BaliseInfo baliseInfo;
      const uint16_t minBaliseWindow = AbstractConfig::corePtr()->getBalWindowMin();
      const uint8_t baliseWindowPermil = AbstractConfig::corePtr()->getBalWindowPermil();
      const uint16_t safetyMargin = DS::AbstractTargets::corePtr()->getSafetyMargin();

      vfwVisitCheckPoint(&beginCp, "ODO_updateBaliseWindow_begin");

      const bool wasFreeRolling = isFreeRolling;
      isFreeRolling = DS::AbstractTargets::corePtr()->getFreeRollingTargetActive();
      if (isFreeRolling && (!wasFreeRolling))
      {
        resetBaliseWindow();
      }

      if (AbstractPosition::corePtr()->getBaliseInfo(baliseInfo))
      {
        trace.write(ATC::detailedTrace, "Check if passed balise was expected, Balise ID: ", static_cast<int32_t>(baliseInfo.nidBG));

        if (AbstractPosition::corePtr()->isPassedBaliseExpected(baliseInfo))
        {
          resetBaliseWindow();
          expectedBalisePassed = true;
        }
      }
      else
      {
        if (PosKnown == AbstractPosition::corePtr()->getAccuracyState())
        {
          // Distance increment since last cycle
          const int32_t dNomIncrement = ATC::ATCMath::instance().absolute(odoData.dNom - dNomOld, __FILE__, __LINE__);
          const int32_t dMaxIncrement = ATC::ATCMath::instance().absolute(odoData.dMax - dMaxOld, __FILE__, __LINE__);
          const int32_t dMinIncrement = ATC::ATCMath::instance().absolute(odoData.dMin - dMinOld, __FILE__, __LINE__);

          // Accumulated distance travelled since last balise passage
          dNomAccumulated += dNomIncrement;
          dMaxAccumulated += dMaxIncrement;
          dMinAccumulated += dMinIncrement;

          const uint32_t maxMinusNom = static_cast<uint32_t>(
            ATC::ATCMath::instance().absolute(dMaxAccumulated - dNomAccumulated, __FILE__, __LINE__));
          const uint32_t nomMinusMin = static_cast<uint32_t>(
            ATC::ATCMath::instance().absolute(dNomAccumulated - dMinAccumulated, __FILE__, __LINE__));
          const uint32_t permilleOfNominal = static_cast<uint32_t>(
            ATC::ATCMath::instance().signDiv(ATC::ATCMath::instance().signMul(
              dNomAccumulated,
              static_cast<uint32_t>(baliseWindowPermil), __FILE__, __LINE__),
              static_cast<int32_t>(1000), __FILE__, __LINE__));

          // Calculate the raw balise window values
          if (isFreeRolling)
          {
            rawLocoEndBaliseWindow = permilleOfNominal;
            rawLastCarBaliseWindow = permilleOfNominal;
          }
          else if (dirMultiplier == negDir) // A end facing cars
          {
            rawLocoEndBaliseWindow = nomMinusMin;
            rawLastCarBaliseWindow = maxMinusNom;
          }
          else // B end facing cars
          {
            rawLocoEndBaliseWindow = maxMinusNom;
            rawLastCarBaliseWindow = nomMinusMin;
          }

          // calculate front balise window bassed on direct of train.
          uint32_t frontBaliseWindow  = rawLocoEndBaliseWindow;
          if (DirReverse == DS::AbstractTargets::corePtr()->getSupposedTravelDir())
          {
            frontBaliseWindow  = rawLastCarBaliseWindow;
          }

          if (frontBaliseWindow > safetyMargin)
          {
            // Set the flag value safetyMarginCrossedFlag, will be used in Position to change the current position.
            safetyMarginCrossedFlag = true;
            // Raise safe brake to stop
            exceededSafetyMargin.setDynamicText(static_cast<uint32_t>(safetyMargin));
            ATC::AbstractEventHandler::corePtr()->reportEvent(exceededSafetyMargin, __FILE__, __LINE__);
          }
        }
      }

      if (rawLocoEndBaliseWindow < minBaliseWindow)
      {
        locoEndBaliseWindow = minBaliseWindow;
      }
      else
      {
        locoEndBaliseWindow = rawLocoEndBaliseWindow;
      }

      if (rawLastCarBaliseWindow < minBaliseWindow)
      {
        lastCarBaliseWindow = minBaliseWindow;
      }
      else
      {
        lastCarBaliseWindow = rawLastCarBaliseWindow;
      }

      vfwVisitCheckPoint(&endCp, "ODO_updateBaliseWindow_end");
    }

    /******************************************************************************
    * processSlipSlideCheck
    ******************************************************************************/
    void AbstractOdometry::processSlipSlideCheck()
    {
      const PosAccuracyState currentAccuracyState = AbstractPosition::corePtr()->getAccuracyState();

      if (expectedBalisePassed || (currentAccuracyState == PosUnknown))
      {
        if (odoData.isSlipping)
        {
          isSlipClear = true;
        }

        expectedBalisePassed = false;
        odoData.isSlipping = false;
        odoData.isSliding = false;

        trace.write(ATC::detailedTrace, "Slip and slide reset at dMax: ", odoData.dMax);
      }
      else
      {
        const bool isSlippingAccordingCOD = ((odoDataRaw.slipSlideStatus1 & slipStatus) > 0U) || ((odoDataRaw.slipSlideStatus2 & slipStatus) > 0U);
        const bool isSlidingAccordingCOD = ((odoDataRaw.slipSlideStatus1 & slideStatus) > 0U) || ((odoDataRaw.slipSlideStatus2 & slideStatus) > 0U);

        if (isSlippingAccordingCOD || (currentAccuracyState == PosApprox))
        {
          if (!odoData.isSlipping)
          { // Only report LogEvent once when slip detected
            ATC::AbstractEventHandler::corePtr()->reportEvent(slipDetected, __FILE__, __LINE__);
          }
          odoData.isSlipping = true;
        }
        // No else: Slip status will be cleared when expected balise is found

        if (isSlidingAccordingCOD)
        {
          if (!odoData.isSliding)
          { // Only report LogEvent once when slide detected
            ATC::AbstractEventHandler::corePtr()->reportEvent(slideDetected, __FILE__, __LINE__);
          }
          odoData.isSliding = true;
        }
        // No else: Slide status will be cleared when expected balise is found
      }
    }

    /******************************************************************************
    * corePtr
    ******************************************************************************/
    AbstractOdometry* AbstractOdometry::corePtr()
    {
      return coreOdometryInstancePtr;
    }

    /******************************************************************************
    * getLocoEndBaliseWindow
    ******************************************************************************/
    uint32_t AbstractOdometry::getLocoEndBaliseWindow() const
    {
      return locoEndBaliseWindow;
    }

    /******************************************************************************
    * getLastCarBaliseWindow
    ******************************************************************************/
    uint32_t AbstractOdometry::getLastCarBaliseWindow() const
    {
      return lastCarBaliseWindow;
    }

    /******************************************************************************
    * getOdoDirection
    ******************************************************************************/
    TravelDir AbstractOdometry::getOdoDirection() const
    {
      return odoData.odoDir;
    }

    /******************************************************************************
    * getOdoPosition
    ******************************************************************************/
    OdoPosition AbstractOdometry::getOdoPosition() const
    {
      return (odoNom + offset);
    }

    /******************************************************************************
    * getOdoTimeStamp
    ******************************************************************************/
    uint32_t AbstractOdometry::getOdoTimeStamp() const
    {
      return odoData.timestamp;
    }

    /******************************************************************************
    * getOdoPositionWithoutOffset
    ******************************************************************************/
    OdoPosition AbstractOdometry::getOdoPositionWithoutOffset() const
    {
      return odoNom;
    }

    /******************************************************************************
    * getOldOdoPositionWithoutOffset
    ******************************************************************************/
    OdoPosition AbstractOdometry::getOldOdoPositionWithoutOffset() const
    {
      return odoNomOld;
    }

    /******************************************************************************
    * getSpeed
    ******************************************************************************/
    uint16_t AbstractOdometry::getSpeed() const
    {
      return absNomSpeed;
    }

    /******************************************************************************
    * getAvgMaxSpeed
    ******************************************************************************/
    uint16_t AbstractOdometry::getFilteredMaxSpeed() const
    {
      uint16_t absMaxSpeed = 0U;
      if(odoData.vNom >= 0)
      {
        absMaxSpeed = static_cast<uint16_t>(ATC::ATCMath::instance().absolute(odoData.vMaxFiltered, __FILE__, __LINE__));
      }
      else
      {
        absMaxSpeed = static_cast<uint16_t>(ATC::ATCMath::instance().absolute(odoData.vMinFiltered, __FILE__, __LINE__));
      }
      return absMaxSpeed;
    }

    /******************************************************************************
    * getAcceleration
    ******************************************************************************/
    int16_t AbstractOdometry::getAcceleration() const
    {
      return odoData.acceleration;
    }

    /******************************************************************************
    * getMaxAccelComp
    ******************************************************************************/
    uint16_t AbstractOdometry::getMaxAccelComp() const
    {
      return maxAccelComp;
    }

    /******************************************************************************
    * isSlipping
    ******************************************************************************/
    bool AbstractOdometry::isSlipping() const
    {
      return odoData.isSlipping;
    }

    /******************************************************************************
    * isSliding
    ******************************************************************************/
    bool AbstractOdometry::isSliding() const
    {
      return odoData.isSliding;
    }

    /******************************************************************************
    * consoleCall
    ******************************************************************************/
    bool AbstractOdometry::consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv)
    {
      // This functions parses the arguments searches for the "help", "trace" or any other Console
      // component specific command calls and handles it. Returns true if completely handled
      // else returns false. returning false will let other components handle the call. help always returns false.

      bool retVal = false;
      char_t  buffer[700];

      // Handle help call at first. argc cannot be 0 as there is a check before consoleCall()
      if (ATC::isTextMatch(&argv[0][0], "help", sizeof("help")) && (argc == 1U))
      {
        const char_t* const helpText =
          "odoVal        Prints the odometry values\n"
          "odoRaw        Prints the raw odometry values\n"
          "odoStat       Prints the odometry statuses\n";

        ATC::AbstractConsole::corePtr()->write(helpText);
        retVal = false;
      }
      else if (ATC::isTextMatch(&argv[0][0], "odoVal", sizeof("odoVal")) && (argc == 1U))
      {
        const int32_t ret = snprintf(&buffer[0], sizeof(buffer),
          "Odo Values:\n"
          "Raw Pos=%d, Odo Pos=%d, Offset=%d, Speed=%u, Odo Dir=%u, Acc=%d, Balise window=+%u -%u, slip=%u, slide=%u",
          odoData.dNom, (odoNom + offset), offset, absNomSpeed, odoData.odoDir,
          dirMultiplier * odoData.acceleration,
          locoEndBaliseWindow, lastCarBaliseWindow, odoData.isSlipping, odoData.isSliding);

        if ((ret > 0)  &&  (static_cast<size_t>(ret)  < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }

        retVal = true;
      }
      else if (ATC::isTextMatch(&argv[0][0], "odoRaw", sizeof("odoRaw")) && (argc == 1U))
      {
        const int32_t ret = snprintf(&buffer[0], sizeof(buffer),
          "Raw odo values (lastValid: %s):\n"
          "slipSlide 1: %u\n"
          "slipSlide 2: %u\n"
          "vTacho1    : %d\n"
          "vTacho2    : %d\n"
          "vDoppl     : %d\n"
          "dTacho1    : %d\n"
          "dTacho2    : %d\n"
          "dDoppl     : %d",
          odoDataRaw.lastValid ? "true" : "false",
          odoDataRaw.slipSlideStatus1, odoDataRaw.slipSlideStatus2,
          odoDataRaw.vTacho1, odoDataRaw.vTacho2, odoDataRaw.vDoppl,
          odoDataRaw.dTacho1, odoDataRaw.dTacho2, odoDataRaw.dDoppl);

        if ((ret > 0)  &&  (static_cast<size_t>(ret)  < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }

        retVal = true;
      }
      else if (ATC::isTextMatch(&argv[0][0], "odoStat", sizeof("odoStat")) && (argc == 1U))
      {
        const int32_t ret = snprintf(&buffer[0], sizeof(buffer), "OdoStatus Values:\n"
          "Q_ODOSAFE: 0 = No service, 1 = Full service  : %u\n"
          "Q_CONTROL:Tachometer 1 in error              : %s\n"
          "Q_CONTROL:Tachometer 2 in error              : %s\n"
          "Q_CONTROL:Doppler radar1 in error            : %s\n"
          "Q_CONTROL:Doppler radar needs maintenance    : %s\n"
          "Q_CONTROL:OSU Agent in error                 : %s\n"
          "Q_CONTROL:OSU in error                       : %s\n"
          "Q_CONTROL:If any axle is slipping            : %s\n"
          "Q_CONTROL:If any axle is sliding             : %s\n"
          "Q_CONTROL:Connection lost to Tachometer 1    : %s\n"
          "Q_CONTROL:Connection lost to Tachometer 2    : %s\n"
          "Q_CONTROL:Connection lost to Doppler radar   : %s\n",
          odoDataStatus.qOdoSafe,
          ((odoDataStatus.qControl & 0x0001U) > 0U) ? "True" : "False",
          ((odoDataStatus.qControl & 0x0002U) > 0U) ? "True" : "False",
          ((odoDataStatus.qControl & 0x0004U) > 0U) ? "True" : "False",
          ((odoDataStatus.qControl & 0x0008U) > 0U) ? "True" : "False",
          ((odoDataStatus.qControl & 0x0010U) > 0U) ? "True" : "False",
          ((odoDataStatus.qControl & 0x0020U) > 0U) ? "True" : "False",
          ((odoDataStatus.qControl & 0x0040U) > 0U) ? "True" : "False",
          ((odoDataStatus.qControl & 0x0080U) > 0U) ? "True" : "False",
          ((odoDataStatus.qControl & 0x0100U) > 0U) ? "True" : "False",
          ((odoDataStatus.qControl & 0x0200U) > 0U) ? "True" : "False",
          ((odoDataStatus.qControl & 0x0400U) > 0U) ? "True" : "False"
        );

        if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }

        retVal = true;
      }
      else if (ATC::isTextMatch(&argv[0][0], "chstat", sizeof("chstat")) && (argc == 1U))
      {
        for (uint8_t cnt = 0U; cnt < numVfwChannelsOdo; cnt++)
        {
          const int32_t ret = snprintf(&buffer[0], sizeof(buffer), "%-30s%-14s%-15u%-12u", chstat[cnt].channelname, chstat[cnt].channelType,
            chstat[cnt].numMsgCnt, chstat[cnt].numMsgBytesCnt);

          if ((ret > 0) && (static_cast<size_t>(ret)  < sizeof(buffer)))
          {
            ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
          }
        }
      }
      else
      {
        // Do nothing
      }

      return retVal;
    }

    /******************************************************************************
    * isTrainStandStill
    ******************************************************************************/
    bool AbstractOdometry::isTrainStandStill() const
    {
      return isTrainStandStillFlag;
    }


    /******************************************************************************
    * getOdometerData
    ******************************************************************************/
    const AbstractOdometry::OdoData& AbstractOdometry::getOdometerData() const
    {
      return odoData;
    }


    /******************************************************************************
    * Health supervision and start Up test for COD
    ******************************************************************************/

    bool AbstractOdometry::startupAndHealthSupTest() const
    {
      return initDone;
    }

    /******************************************************************************
    * checkTrainStandStill
    ******************************************************************************/
    void AbstractOdometry::checkTrainStandStill(void)
    {
      if (0U == getSpeed())
      {
        trainNotAtStandStillCounter = 0U;

        if (trainStandStillCounter < maxCountTrainStandStill)
        {
          trainStandStillCounter++;
        }
        else
        {
          isTrainStandStillFlag = true;
        }
      }
      else
      {
        trainStandStillCounter = 0U;
         
        // Move out of stand still when train movement is detected for half the Max standstill counter value.
        if (trainNotAtStandStillCounter < (maxCountTrainStandStill / 2U))
        {
          trainNotAtStandStillCounter++;
        }
        else
        {
          isTrainStandStillFlag = false;
        }

      }
    }

    /******************************************************************************
    * isSafetyMarginCrossed
    ******************************************************************************/
    bool AbstractOdometry::isSafetyMarginCrossed() const
    {
      return safetyMarginCrossedFlag;
    }

    /******************************************************************************
    * isSlipStatusChanged
    ******************************************************************************/
    bool AbstractOdometry::isSlipStatusClear() const
    {
      return isSlipClear;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void AbstractOdometry::initCrossCompare() const
    {
      Support::AbstractCrossCompare* const cc = Support::AbstractCrossCompare::corePtr();
      //lint --e{586} 'new' is acceptable during initialization

      // lastLogCODDetails : not vital
      // chstat[numVfwChannelsOdo] : not vital
      cc->addCrossCompareData(new Support::CrossCompareBool(&initDone));
      cc->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&configFailure));
      cc->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&invalidState));
      cc->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&noConfigResponse));
      cc->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&noMeasurementData));
      cc->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&invalidMeasurementData));
      cc->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&versionCODMismatch));
      cc->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&exceededSafetyMargin));
      cc->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&odometerSpeedSensorFailureOccurred));
      cc->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&odometerSpeedSensorFailureRecovered));
      cc->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&tachometer1FailureOccured));
      cc->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&tachometer2FailureOccured));
      cc->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&dopplerFailureOccured));
      cc->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&dopplerRadarNeedsMaintenance));
      cc->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&tachometer1FailureRecovered));
      cc->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&tachometer2FailureRecovered));
      cc->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&dopplerFailureRecovered));


      cc->addCrossCompareData(new Support::CrossCompareEnum<OdometerConfigState>(&odometerConfigState));
      cc->addCrossCompareData(new Support::CrossCompareBool(&odometerConfigSent));
      cc->addCrossCompareData(new Support::CrossCompareInt64(&odometerConfigSentTime));
      cc->addCrossCompareData(new Support::CrossCompareUint32(&locoEndBaliseWindow));
      cc->addCrossCompareData(new Support::CrossCompareUint32(&lastCarBaliseWindow));
      cc->addCrossCompareData(new Support::CrossCompareUint32(&rawLocoEndBaliseWindow));
      cc->addCrossCompareData(new Support::CrossCompareUint32(&rawLastCarBaliseWindow));
      cc->addCrossCompareData(new Support::CrossCompareInt32(&dNomAccumulated));
      cc->addCrossCompareData(new Support::CrossCompareInt32(&dMaxAccumulated));
      cc->addCrossCompareData(new Support::CrossCompareInt32(&dMinAccumulated));
      cc->addCrossCompareData(new Support::CrossCompareInt32(&odoNom));
      cc->addCrossCompareData(new Support::CrossCompareInt32(&odoNomOld));
      cc->addCrossCompareData(new Support::CrossCompareInt32(&offset));
      cc->addCrossCompareData(new Support::CrossCompareInt8(&dirMultiplier));
      cc->addCrossCompareData(new Support::CrossCompareUint16(&maxAccelComp));

      // odoData
      cc->addCrossCompareData(new Support::CrossCompareUint16(&odoData.qControl));
      cc->addCrossCompareData(new Support::CrossCompareBool(&odoData.isOdoSafe));
      cc->addCrossCompareData(new Support::CrossCompareBool(&odoData.dirError));
      cc->addCrossCompareData(new Support::CrossCompareUint32(&odoData.timestamp));
      cc->addCrossCompareData(new Support::CrossCompareUint32(&odoData.tProdTime));
      cc->addCrossCompareData(new Support::CrossCompareInt16(&odoData.acceleration));
      cc->addCrossCompareData(new Support::CrossCompareInt16(&odoData.vNom));
      cc->addCrossCompareData(new Support::CrossCompareInt16(&odoData.vMax));
      cc->addCrossCompareData(new Support::CrossCompareInt16(&odoData.vMin));
      cc->addCrossCompareData(new Support::CrossCompareInt32(&odoData.dNom));
      cc->addCrossCompareData(new Support::CrossCompareInt32(&odoData.dMax));
      cc->addCrossCompareData(new Support::CrossCompareInt32(&odoData.dMin));
      cc->addCrossCompareData(new Support::CrossCompareBool(&odoData.isSlipping));
      cc->addCrossCompareData(new Support::CrossCompareBool(&odoData.isSliding));
      cc->addCrossCompareData(new Support::CrossCompareEnum<TravelDir>(&odoData.odoDir));
      cc->addCrossCompareData(new Support::CrossCompareEnum<TravelDir>(&odoData.odoDirOld));

      // Odoval (odoNom + offset) registered for AOS Analyzer
      cc->addCrossCompareData(new Support::CrossCompareInt32(&odoValForAnalyser));

      // Absolute value of speed published to other components and registered for AOS Analyzer 
      cc->addCrossCompareData(new Support::CrossCompareUint16(&absNomSpeed));
      cc->addCrossCompareData(new Support::CrossCompareUint8(&odoDataRaw.slipSlideStatus1));
      cc->addCrossCompareData(new Support::CrossCompareUint8(&odoDataRaw.slipSlideStatus2));
      // odoDataStatus : not vital
      cc->addCrossCompareData(new Support::CrossCompareUint8(&odoDynamicConfig.gradCurrent));
      cc->addCrossCompareData(new Support::CrossCompareUint8(&odoDynamicConfig.gradDir));
      cc->addCrossCompareData(new Support::CrossCompareUint8(&odoDynamicConfig.gradType));
      cc->addCrossCompareData(new Support::CrossCompareInt32(&odoDynamicConfig.gradPrev));
      cc->addCrossCompareData(new Support::CrossCompareBool(&isTrainStandStillFlag));
      cc->addCrossCompareData(new Support::CrossCompareUint8(&trainStandStillCounter));
      cc->addCrossCompareData(new Support::CrossCompareUint8(&trainNotAtStandStillCounter));
      cc->addCrossCompareData(new Support::CrossCompareBool(&safetyMarginCrossedFlag));
      cc->addCrossCompareData(new Support::CrossCompareBool(&isSlipClear));
      cc->addCrossCompareData(new Support::CrossCompareBool(&isFreeRolling));
      cc->addCrossCompareData(new Support::CrossCompareInt32(&dNomOld));
      cc->addCrossCompareData(new Support::CrossCompareInt32(&dMaxOld));
      cc->addCrossCompareData(new Support::CrossCompareInt32(&dMinOld));

      //SDP Version
      cc->addCrossCompareData(new Support::CrossCompareUint8(&sdpMajorVersion));
      cc->addCrossCompareData(new Support::CrossCompareUint8(&sdpMiddleVersion));
      cc->addCrossCompareData(new Support::CrossCompareUint8(&sdpMinorVersion));

      cc->addCrossCompareData(new Support::CrossCompareBool(&expectedBalisePassed));
      cc->addCrossCompareData(new Support::CrossCompareBool(&odometerSpeedSensorFailure));
    }

    /******************************************************************************
    * getTachometer1Failure
    ******************************************************************************/
    bool AbstractOdometry::getTachometer1Failure() const
    {
        return tachometer1Failure;
    }


    /******************************************************************************
    * getTachometer2Failure
    ******************************************************************************/
    bool AbstractOdometry::getTachometer2Failure() const
    {
      return tachometer2Failure;
    }

    /******************************************************************************
    * getDopplerFailure
    ******************************************************************************/
    bool AbstractOdometry::getDopplerFailure() const
    {
      return dopplerFailure;

    }

    /******************************************************************************
    * evaluateOdometerSensorFailure
    ******************************************************************************/
    void AbstractOdometry::evaluateOdometerSensorFailure()
    {

      const bool tachometer1Error         =  (((odoDataStatus.qControl & 0x0001U) > 0U) ? true : false);
      const bool tachometer2Error          = (((odoDataStatus.qControl & 0x0002U) > 0U) ? true : false);
      const bool dopplerError              = (((odoDataStatus.qControl & 0x0004U) > 0U) ? true : false);
      const bool dopplerNeedsMaintenance   = (((odoDataStatus.qControl & 0x0008U) > 0U) ? true : false);
      const bool tachometer1ConnectionLost = (((odoDataStatus.qControl & 0x0100U) > 0U) ? true : false);
      const bool tachometer2ConnectionLost = (((odoDataStatus.qControl & 0x0200U) > 0U) ? true : false);
      const bool dopplerConnectionLost     = (((odoDataStatus.qControl & 0x0400U) > 0U) ? true : false);

      if ((tachometer1Error) || (tachometer1ConnectionLost))
      {
        if (!tachometer1Failure)
        {
          tachometer1Failure = true;
          ATC::AbstractEventHandler::corePtr()->reportEvent(tachometer1FailureOccured, __FILE__, __LINE__);
        }
      }
      else
      {
        if (tachometer1Failure)
        {
          tachometer1Failure = false;
          ATC::AbstractEventHandler::corePtr()->reportEvent(tachometer1FailureRecovered, __FILE__, __LINE__);
        }
      }

      if ((tachometer2Error) || (tachometer2ConnectionLost))
      {
        if (!tachometer2Failure)
        {
          tachometer2Failure = true;
          ATC::AbstractEventHandler::corePtr()->reportEvent(tachometer2FailureOccured, __FILE__, __LINE__);
          writeToLog(ATC::BriefLog,"Failure detected in Tachometer1 Q_CONTROL VALUE:", static_cast<int32_t>(odoDataStatus.qControl), __FILE__, __LINE__);

        }
      }
      else
      {
        if (tachometer2Failure)
        {
          tachometer2Failure = false;
          ATC::AbstractEventHandler::corePtr()->reportEvent(tachometer2FailureRecovered, __FILE__, __LINE__);
          writeToLog(ATC::BriefLog, "Failure detected in Tachometer2 Q_CONTROL VALUE:", static_cast<int32_t>(odoDataStatus.qControl), __FILE__, __LINE__);

        }
      }

      if ((dopplerError) || (dopplerConnectionLost) || (dopplerNeedsMaintenance))
      {
        if (!dopplerFailure)
        {
          dopplerFailure = true;
          ATC::AbstractEventHandler::corePtr()->reportEvent(dopplerFailureOccured, __FILE__, __LINE__);
          writeToLog(ATC::BriefLog, "Failure detected in Doppler Q_CONTROL VALUE:",static_cast<int32_t>(odoDataStatus.qControl), __FILE__, __LINE__);

          //when the Doppler radar has been invalid for more than 10 % during one hour.
          //Doppler needs maintenance event is reported.
          if (dopplerNeedsMaintenance)
          {
            ATC::AbstractEventHandler::corePtr()->reportEvent(dopplerRadarNeedsMaintenance, __FILE__, __LINE__);
            writeToLog(ATC::BriefLog, "Maintenance needed for Doppler Q_CONTROL VALUE:", static_cast<int32_t>(odoDataStatus.qControl), __FILE__, __LINE__);

          }
        }
      }
      else
      {
        if (dopplerFailure)
        {
          dopplerFailure = false;
          ATC::AbstractEventHandler::corePtr()->reportEvent(dopplerFailureRecovered, __FILE__, __LINE__);
        }
      }

    }

    /******************************************************************************
    * setSDPVersion
    ******************************************************************************/

    void AbstractOdometry::setSDPVersion(const uint8_t sdpMajorVer, const uint8_t sdpMiddleVer, const uint8_t sdpMinorVer)
    {
      sdpMajorVersion = sdpMajorVer;
      sdpMiddleVersion = sdpMiddleVer;
      sdpMinorVersion = sdpMinorVer;
    }

    /******************************************************************************
    * getSDPVersion
    ******************************************************************************/
    void AbstractOdometry::getSDPVersion(uint8_t &sdpMajorVer, uint8_t &sdpMiddleVer, uint8_t &sdpMinorVer) const
    {
      sdpMajorVer = sdpMajorVersion;
      sdpMiddleVer = sdpMiddleVersion;
      sdpMinorVer = sdpMinorVersion;
    }

    /******************************************************************************
    * logCODDetails
    ******************************************************************************/
    void AbstractOdometry::logCODDetails(const bool logNow)
    {
      int32_t distSinceLastLog = ATC::ATCMath::instance().absolute(odoData.dNom - lastLogCODDetails, __FILE__, __LINE__);
      if (logNow || (distSinceLastLog > 50000L))
      {
        char_t logBuf[100];
        const int32_t res = snprintf(&logBuf[0], sizeof(logBuf), "vNom = %d, vMax = %d, vMin = %d, dNom = %d, dMax = %d, dMin = %d",
          odoData.vNom, odoData.vMax, odoData.vMin, odoData.dNom, odoData.dMax, odoData.dMin);

        if ((res > 0)  &&  (static_cast<size_t>(res)  < sizeof(logBuf)))
        {
          writeToLog(ATC::DetailedLog, &logBuf[0], __FILE__, __LINE__);
        }

        lastLogCODDetails = odoData.dNom;
      }
    }
  }
}

//lint +esym(586,snprintf)
