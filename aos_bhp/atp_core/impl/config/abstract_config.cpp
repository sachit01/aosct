/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of AbstractConfig, the ATP-Core part of Config.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-06-10    jeneman     Created
* 2016-09-21    nsyed       Added individual access functions for config params
* 2016-10-06    arastogi    Added core ptr function.
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "abstract_config.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include "atc_math.hpp"

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
  /******************************************************************************
  * Constructor
  ******************************************************************************/
  AbstractConfig::AbstractConfig() : ATC::AbstractConfigBase(), coreInitDone(false)
  {

  }

  /******************************************************************************
  * init
  ******************************************************************************/
  bool AbstractConfig::init(void)
  {
    bool retValue = false;
    // Config init will be run multiple times until all config files are read, but this part should only be run once.
    if (!coreInitDone)
    {
      // Add ATP-Core config items here
      // Note! The minimum/maximum values for parameters with a "global name" must match the config parameter ranges specified in FFFIS TCC-AOS
      //                  ID            Name          Description                   Unit      Min    Max  Default         Read-only  MemoryArea
      addIPaddrConfigItem(ATC::njruIpId, "NjruIp", "NJRU IP address",
        "IP", "192.168.2.30", configFileCommon);
      addUint16ConfigItem(ATC::njruPortId, "NjruPort", "NJRU Port",
        "port", 30000U, 65535U, 30131U, configFileCommon);
      addUint16ConfigItem(ATC::bdsPortId, "BdsPort", "BDS Port",
        "port", 1U, 65535U, 5514U, configFileCommon);
      addUint16ConfigItem(ATC::consolePortId, "ConsolePort", "Console Port",
        "port", 30000U, 65535U, 30165U, configFileCommon);
      addUint8ConfigItem(ATC::bdsLevelId, "BdsLevel", "BDS log-level",
        "level", 0U, 9U, 0U, configFileCommon);
      addUint8ConfigItem(ATC::njruLevelId, "NjruLevel", "NJRU log-level",
        "level", 0U, 9U, 1U, configFileCommon);
      addIPaddrConfigItem(ATC::ruIpId, "RuIp", "Recording unit (RU) IP address",
        "IP", "192.168.2.30", configFileCommon);
      addUint16ConfigItem(ATC::ruPortId, "RuPort", "Recording unit (RU) port",
        "port", 30000U, 65535U, 30180U, configFileCommon);
      addUint8ConfigItem(ATC::ruLogDynValuePeriodId, "RuLogDynValuePeriod",
        "Period for logging dynamic values", "s", 0U, 255U, 5U, configFileCommon);
      addUint16ConfigItem(ATC::ruLogValueDiffId, "RuLogValueDiff",
        "Min. diff. for logging analog values", "none", 0U, 65535U, 30U, configFileCommon);
      addUint16ConfigItem(balNidCId, "BalNidC", "Eurobalise country code",
        "nr", 0U, 1023U, 530U, configFileCommon);
      addUint16ConfigItem(radioIdId, "RadioId", "Radio ID", "nr",
        0U, 65535U, 2U, configFileInstance);
      addUint8ConfigItem(siteIdId, "SiteId", "Site ID",
        "nr", 0U, 255U, 6U, configFileCommon);
      addUint8ConfigItem(radioTimeOutId, "RadioTimeOut", "Time elapsed without any message received before connection with TCC is considered broken",
        "s", 5U, 30U, 20U, configFileCommon);
      addUint8ConfigItem(radioTimeOutSbId, "RadioTimeOutSb", "Time elapsed after detecting a lost radio connection until SB is applied",
        "s", 0U, 60U, 2U, configFileCommon);
      addUint16ConfigItem(balSearchDistanceId, "BalSearchDistance", "Maximum allowed distance to travel while looking for first balise",
        "cm", 2000U, 20000U, 4900U, configFileCommon);
      addUint16ConfigItem(balSearchSpeedId, "BalSearchSpeed", "Maximum allowed speed during balise search",
        "cm/s", 50U, 200U, 83U, configFileCommon);
      addUint16ConfigItem(balWindowMinId, "BalWindowMin", "Minimum size of balise window",
        "cm", 250U, 5000U, 500U, configFileCommon);
      addUint8ConfigItem(balWindowPermilId, "BalWindowPermil", "Balise window in permil of distance travelled since last balise",
        "permil", 2U, 100U, 40U, configFileType);
      addUint16ConfigItem(yardSpeedId, "YardSpeed", "Allowed speed in YardMode when no TCC communication was established",
        "cm/s", 70U, 65535U, 72U, configFileType);
      addBoolConfigItem(atoEnableId, "AtoEnable", "ATO Functionality Status",
        "bool", false, configFileType);
      addUint16ConfigItem(revSupMarginNormId, "RevSupMarginNorm", "Reversing supervision margin for Normal, Staff Resp, Split, Join and Bal Search",
        "cm", 50U, 3000U, 100U, configFileCommon);
      addUint16ConfigItem(revSupMarginShuntId, "RevSupMarginShunt", "Reversing supervision margin in shunting Route",
        "cm", 50U, 5000U, 100U, configFileCommon);
      addUint16ConfigItem(rollAwayMarginId, "RollAwayMargin", "Maximum allowed roll-away movement",
        "cm", 50U, 3000U, 100U, configFileCommon);
      addUint16ConfigItem(tachoPulsesPer10Rev1Id, "TachoPulsesPer10Rev1", "Number of tachometer pulses per 10 revolutions of wheel 1",
        "pulses per 10 rev", 200U, 20000U, 6121U, configFileType);
      addUint16ConfigItem(tachoPulsesPer10Rev2Id, "TachoPulsesPer10Rev2", "Number of tachometer pulses per 10 revolutions of wheel 2",
        "pulses per 10 rev", 200U, 20000U, 6121U, configFileType);
      addUint16ConfigItem(wheelSize1Id, "WheelSize1", "Actual wheelSize 1",
        "mm", 150U, 1500U, 1092U, configFileMaint);
      addUint16ConfigItem(wheelSize2Id, "WheelSize2", "Actual wheelSize 2",
        "mm", 150U, 1500U, 1092U, configFileMaint);
      addUint16ConfigItem(balAntennaPosEndId, "BalAntennaPosEnd", "Balise antenna position end",
        "cm", 0U, 30000U, 100U, configFileType);
      addUint16ConfigItem(balAntennaPosFrontId, "BalAntennaPosFront", "Balise antenna position front",
        "cm", 0U, 30000U, 1900U, configFileType);
      addBoolConfigItem(enableDiagnosticsId, "EnableDiagnostics", "Enable / disable console and Analyzer (1 = enable, 0 = disable)",
        "bool", true, configFileCommon);
      addUint8ConfigItem(maxWheelSizeErrorId, "MaxWheelSizeError",
        "Max estimated wheel size error. I.e. the difference between WheelSize1/2 and the real wheel size", "permil", 1U, 30U, 10U, configFileCommon);
      addUint16ConfigItem(revSupMarginFreeRollingId, "RevSupMarginFreeRoll", "Reversing supervision margin in Free Rolling",
        "cm", 50U, 5000U, 100U, configFileCommon);
      addUint16ConfigItem(minSpeedMarginWarnId, "MinSpeedMarginWarn", "Minimum speed margin over ceiling speed before warning",
        "cm/s", 1U, 28U, 17U, configFileCommon);
      addUint16ConfigItem(minSpeedMarginSBId, "MinSpeedMarginSb", "Minimum speed margin over ceiling speed before SB",
        "cm/s", 6U, 139U, 83U, configFileCommon);
      addUint16ConfigItem(minSpeedMarginEBId, "MinSpeedMarginEb", "Minimum speed margin over ceiling speed before EB",
        "cm/s", 28U, 139U, 139U, configFileCommon);
      addUint16ConfigItem(maxSpeedMarginWarnId, "MaxSpeedMarginWarn", "Maximum speed margin over ceiling speed before warning",
        "cm/s", 14U, 139U, 83U, configFileCommon);
      addUint16ConfigItem(maxSpeedMarginSBId, "MaxSpeedMarginSb", "Maximum speed margin over ceiling speed before SB",
        "cm/s", 28U, 278U, 139U, configFileCommon);
      addUint16ConfigItem(maxSpeedMarginEBId, "MaxSpeedMarginEb", "Maximum speed margin over ceiling speed before EB",
        "cm/s", 33U, 417U, 333U, configFileCommon);
      addUint16ConfigItem(speedMarginWarnId, "SpeedMarginWarn", "Permil of ceiling speed, Speed margin over ceiling speed before warning",
        "permil", 20U, 100U, 50U, configFileCommon);
      addUint16ConfigItem(speedMarginSBId, "SpeedMarginSb", "Permil of ceiling speed, speed margin over ceiling speed before SB",
        "permil", 30U, 150U, 100U, configFileCommon);
      addUint16ConfigItem(speedMarginEBId, "SpeedMarginEb", "Permil of ceiling speed, speed margin over ceiling speed before EB",
        "permil", 50U, 300U, 200U, configFileCommon);
      addUint16ConfigItem(fwToSwDelayId, "FwToSwDelay", "Delay time from first warning curve to second warning",
        "s", 1U, 15U, 2U, configFileCommon);
      addUint16ConfigItem(swToSBDelayId, "SwToSbDelay", "Delay time from second warning curve to service brake application curve",
        "s", 1U, 15U, 2U, configFileCommon);
      addUint16ConfigItem(sbToEbDelayId, "SbToEbDelay", "Internal AOS delay time from service brake application until emergency brake application ",
        "0.1 s", 0U, 100U, 30U, configFileCommon);
      addUint16ConfigItem(effectiveEbDelayId, "EffectiveEbDelay", "Internal AOS delay in applying emergency brake",
        "0.1 s", 0U, 100U, 50U, configFileCommon);
      addUint8ConfigItem(opcMajorVersionId, "OpcMajorVersion", "Expected OPC Major Version",
        "nr", 0U, 255U, 4U, configFileCommon);
      addUint8ConfigItem(opcMiddleVersionId, "OpcMiddleVersion", "Expected OPC Middle Version",
        "nr", 0U, 255U, 2U, configFileCommon);
      addUint8ConfigItem(opcMinorVersionId, "OpcMinorVersion", "Expected OPC Minor Version",
        "nr", 0U, 255U, 10U, configFileCommon);
      addUint16ConfigItem(analyzerIFPortId, "AnalyzerIfPort", "Analyzer Port",
        "port", 30000U, 65535U, 30160U, configFileCommon);
      addUint8ConfigItem(sendCycleAIFId, "SendCycleAif", "No of cycles  between sending of measured values to Analyzer",
        "nr", 5U, 50U, 10U, configFileCommon);
      addUint16ConfigItem(secBalSearchDistanceId, "SecBalSearchDistance",
        "Allowed distance to travel while waiting for an MA to search for the second registration balise, from position of first detected balise.",
        "cm", 2000U, 20000U, 4500U, configFileCommon);
      addUint8ConfigItem(eventKeepActiveTimeId, "EventKeepActiveTime", "The time an event is kept active after the trigger is removed",
        "s", 0U, 20U, 5U, configFileCommon);
      addUint16ConfigItem(trainCfgTicTimeoutId, "TrainCfgTicTimeout", "Time out for Train Configuration with TIC system",
        "s", 10U, 1800U, 1800U, configFileType);
      addUint8ConfigItem(codSensorConfigId, "CodSensorConfig", "COD sensor configuration",
        "range", 1U, 2U, 2U, configFileType);
      addUint8ConfigItem(tractionControlId, "TractionControl", "Type of traction system with intentional slipping and/or sliding mounted.",
        "range", 0U, 3U, 2U, configFileType);
      addUint16ConfigItem(minDopplerSpeedId, "MinDopplerSpeed", "Minimum speed when Doppler shall be used",
        "km/h", 0U, 500U, 0U, configFileType);
      addUint16ConfigItem(maxDopplerSpeedId, "MaxDopplerSpeed", "Maximum speed when Doppler shall be used",
        "km/h", 0U, 500U, 500U, configFileType);
      addUint16ConfigItem(maxDopplerAccelId, "MaxDopplerAccel", "Maximum acceleration that Doppler radar can handle",
        "cm/s2", 0U, 400U, 400U, configFileType);
      addUint32ConfigItem(dopplerPulsesPerKilometerId, "DopplerPulsesPerKm", "Number of Doppler radar pulses per km to distance travelled",
        "pulse/km", 0U, 260000U, 26172U, configFileMaint);
      addUint8ConfigItem(dopplerPulsePrecisionId, "DopplerPulsePrec", "Maximum error in the precision of Doppler pulse rate.",
        "permil", 0U, 100U, 1U, configFileType);
      addUint16ConfigItem(btmTestNotifyId, "BtmTestNotify",
        "Perform or notify the driver to initiate a BTM routine test when more than the BTM test notification time has elapsed since last"
        "successful test", "min", 10U, 5760U, 5760U, configFileCommon);
      addUint16ConfigItem(btmTestMandatoryId, "BtmTestMandatory",
        "A successful BTM routine test must be performed before the BTM test mandatory time has elapsed since the last successful test",
        "min", 10U, 10080U, 10080U, configFileCommon);
      addUint32ConfigItem(lastBTMTestTimeId, "LastBtmTestTime", "Timestamp of last performed successful BTM test",
        "s", 0U, 4294967295U, 0U, configFileRuntime);
      addUint16ConfigItem(brakeTestNotifyId, "BrakeTestNotify",
        "Notify the driver to initiate a Brake test when more than the Brake test notification time has elapsed since last successful test",
        "min", 10U, 5760U, 5760U, configFileCommon);
      addUint16ConfigItem(brakeTestMandatoryId, "BrakeTestMandatory",
        "A successful Brake test must be performed before the Brake test mandatory time has elapsed since the last successful test",
        "min", 10U, 9600U, 9600U, configFileCommon);
      addUint32ConfigItem(lastBrakeTestTimeId, "LastBrakeTestTime", "Last performed time stamp for Brake test",
        "s", 0U, 4294967295U, 0U, configFileRuntime);
      addUint16ConfigItem(brakeTestExecTimeId, "BrakeTestExecTime", "Max execution time for a complete Brake Test",
        "s", 1U, 3600U, 300U, configFileType);
      addUint8ConfigItem(tacho1DirectionId, "Tacho1Direction", "The rotating direction of Tacho 1 (1: pos, 2: neg)",
        "nr", 1U, 2U, 1U, configFileType);
      addUint8ConfigItem(tacho2DirectionId, "Tacho2Direction", "The rotating direction of Tacho 2 (1: pos, 2: neg)",
        "nr", 1U, 2U, 2U, configFileType);
      addUint32ConfigItem(brakeTestReasonId, "BrakeTestReason", "Number code defining the reason for a Mandatory Brake Test",
        "nr", 0U, 20U, 4U, configFileRuntime);
      addUint16ConfigItem(maxTimeAOSRunId, "MaxTimeAosRun", "Maximum time AOS is allowed to run continuously without reset",
        "hrs", 1U, 1080U, 240U, configFileCommon);
      addUint16ConfigItem(intRelaysTimeoutId, "IntRelaysTimeout", "Max wait time for the internal relays",
        "ms", 100U, 3000U, 700U, configFileCommon);
      addUint8ConfigItem(sbExpectedDecLimitId, "SbExpectedDecLimit",
        "Percentage of expected deceleration below which deceleration is not sufficient for Service Brake application ",
        "%", 40U, 100U, 60U, configFileCommon);
      addUint8ConfigItem(ebExpectedDecLimitId, "EbExpectedDecLimit",
        "Percentage of expected deceleration below which deceleration is not sufficient for Emergency Brake application",
        "%", 40U, 100U, 60U, configFileCommon);
      addUint8ConfigItem(locoTypeId, "LocoType", "Locomotive type",
        "nr", getMinValLocoType(), getMaxValLocoType(), getDefaultValLocoType(), configFileType);      
      addUint8ConfigItem(cabinConfigurationId, "CabinConfiguration", "Configuration for one or two cabins",
        "nr", 1U, 2U, 2U, configFileType);
      addBoolConfigItem(esaInputAvailId, "EsaInputAvail", "Emergency Stop input available",
        "bool", false, configFileType);
      addBoolConfigItem(sbAvailId, "SbAvail", "Service Brake access available",
        "bool", true, configFileType);
      addUint16ConfigItem(brakeType1ParamA1Id, "BrakeType1ParamA1", "Brake Parameter A1 for Brake System Type 1",
        "0.01cm/s2", 0U, 5000U, 251U, configFileType);
      addUint16ConfigItem(brakeType2ParamA1Id, "BrakeType2ParamA1", "Brake Parameter A1 for Brake System Type 2",
        "0.01cm/s2", 0U, 5000U, 251U, configFileType);
      addUint16ConfigItem(brakeType3ParamA1Id, "BrakeType3ParamA1", "Brake Parameter A1 for Brake System Type 3",
        "0.01cm/s2", 0U, 5000U, 251U, configFileType);
      addUint16ConfigItem(brakeType1ParamA2Id, "BrakeType1ParamA2", "Brake Parameter A2 for Brake System Type 1",
        "0.01cm/s2", 0U, 5000U, 215U, configFileType);
      addUint16ConfigItem(brakeType2ParamA2Id, "BrakeType2ParamA2", "Brake Parameter A2 for Brake System Type 2",
        "0.01cm/s2", 0U, 5000U, 215U, configFileType);
      addUint16ConfigItem(brakeType3ParamA2Id, "BrakeType3ParamA2", "Brake Parameter A2 for Brake System Type 3",
        "0.01cm/s2", 0U, 5000U, 215U, configFileType);
      addUint16ConfigItem(brakeType1ParamA3Id, "BrakeType1ParamA3", "Brake Parameter A3 for Brake System Type 1",
        "0.01cm/s2", 0U, 5000U, 191U, configFileType);
      addUint16ConfigItem(brakeType2ParamA3Id, "BrakeType2ParamA3", "Brake Parameter A3 for Brake System Type 2",
        "0.01cm/s2", 0U, 5000U, 191U, configFileType);
      addUint16ConfigItem(brakeType3ParamA3Id, "BrakeType3ParamA3", "Brake Parameter A3 for Brake System Type 3",
        "0.01cm/s2", 0U, 5000U, 191U, configFileType);
      addUint16ConfigItem(brakeType1ParamB1Id, "BrakeType1ParamB1", "Brake Parameter B1 for Brake System Type 1",
        "0.01cm/s2", 0U, 5000U, 691U, configFileType);
      addUint16ConfigItem(brakeType2ParamB1Id, "BrakeType2ParamB1", "Brake Parameter B1 for Brake System Type 2",
        "0.01cm/s2", 0U, 5000U, 691U, configFileType);
      addUint16ConfigItem(brakeType3ParamB1Id, "BrakeType3ParamB1", "Brake Parameter B1 for Brake System Type 3",
        "0.01cm/s2", 0U, 5000U, 691U, configFileType);
      addUint16ConfigItem(brakeType1ParamB2Id, "BrakeType1ParamB2", "Brake Parameter B2 for Brake System Type 1",
        "0.01cm/s2", 0U, 5000U, 626U, configFileType);
      addUint16ConfigItem(brakeType2ParamB2Id, "BrakeType2ParamB2", "Brake Parameter B2 for Brake System Type 2",
        "0.01cm/s2", 0U, 5000U, 626U, configFileType);
      addUint16ConfigItem(brakeType3ParamB2Id, "BrakeType3ParamB2", "Brake Parameter B2 for Brake System Type 3",
        "0.01cm/s2", 0U, 5000U, 626U, configFileType);
      addUint16ConfigItem(brakeType1ParamB3Id, "BrakeType1ParamB3", "Brake Parameter B3 for Brake System Type 1",
        "0.01cm/s2", 0U, 5000U, 423U, configFileType);
      addUint16ConfigItem(brakeType2ParamB3Id, "BrakeType2ParamB3", "Brake Parameter B3 for Brake System Type 2",
        "0.01cm/s2", 0U, 5000U, 423U, configFileType);
      addUint16ConfigItem(brakeType3ParamB3Id, "BrakeType3ParamB3", "Brake Parameter B3 for Brake System Type 3",
        "0.01cm/s2", 0U, 5000U, 423U, configFileType);
      addUint16ConfigItem(brakeType1ParamV1Id, "BrakeType1ParamV1", "Brake Parameter V1 for Brake System Type 1",
        "cm/s", 0U, 3500U, 555U, configFileType);
      addUint16ConfigItem(brakeType2ParamV1Id, "BrakeType2ParamV1", "Brake Parameter V1 for Brake System Type 2",
        "cm/s", 0U, 3500U, 555U, configFileType);
      addUint16ConfigItem(brakeType3ParamV1Id, "BrakeType3ParamV1", "Brake Parameter V1 for Brake System Type 3",
        "cm/s", 0U, 3500U, 555U, configFileType);
      addUint16ConfigItem(brakeType1ParamV2Id, "BrakeType1ParamV2", "Brake Parameter V2 for Brake System Type 1",
        "cm/s", 0U, 3500U, 1111U, configFileType);
      addUint16ConfigItem(brakeType2ParamV2Id, "BrakeType2ParamV2", "Brake Parameter V2 for Brake System Type 2",
        "cm/s", 0U, 3500U, 1111U, configFileType);
      addUint16ConfigItem(brakeType3ParamV2Id, "BrakeType3ParamV2", "Brake Parameter V2 for Brake System Type 3",
        "cm/s", 0U, 3500U, 1111U, configFileType);
      addUint8ConfigItem(brakeType1LambdaMinId, "BrakeType1LambdaMin", "Minimum lambda for Brake Type 1",
        "%", 0U, 100U, 9U, configFileType);
      addUint8ConfigItem(brakeType2LambdaMinId, "BrakeType2LambdaMin", "Minimum lambda for Brake Type 2",
        "%", 0U, 100U, 9U, configFileType);
      addUint8ConfigItem(brakeType3LambdaMinId, "BrakeType3LambdaMin", "Minimum lambda for Brake Type 3",
        "%", 0U, 100U, 9U, configFileType);
      addUint8ConfigItem(brakeType1LambdaMaxId, "BrakeType1LambdaMax", "Maximum lambda for Brake Type 1",
        "%", 0U, 100U, 90U, configFileType);
      addUint8ConfigItem(brakeType2LambdaMaxId, "BrakeType2LambdaMax", "Maximum lambda for Brake Type 2",
        "%", 0U, 100U, 80U, configFileType);
      addUint8ConfigItem(brakeType3LambdaMaxId, "BrakeType3LambdaMax", "Maximum lambda for Brake Type 3",
        "%", 0U, 100U, 70U, configFileType);
      addUint16ConfigItem(maxAccelerationId, "MaxAcceleration", "Maximum expected acceleration value",
        "cm/s2", 0U, 400U, 20U, configFileType);
      addUint16ConfigItem(maxDecelerationId, "MaxDeceleration", "Maximum expected deceleration value",
        "cm/s2", 0U, 400U, 50U, configFileType);
      addStringConfigItem(vehicleType1Id, "VehicleType1", "Vehicle type 1 to be used in DMI for Manual configuration",
        "str", 1U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(vehicleType2Id, "VehicleType2", "Vehicle type 2 to be used in DMI for Manual configuration",
        "str", 1U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(vehicleType3Id, "VehicleType3", "Vehicle type 3 to be used in DMI for Manual configuration",
        "str", 1U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(vehicleType4Id, "VehicleType4", "Vehicle type 4 to be used in DMI for Manual configuration",
        "str", 1U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(vehicleType5Id, "VehicleType5", "Vehicle type 5 to be used in DMI for Manual configuration",
        "str", 1U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(vehicleType6Id, "VehicleType6", "Vehicle type 6 to be used in DMI for Manual configuration",
        "str", 1U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(vehicleType7Id, "VehicleType7", "Vehicle type 7 to be used in DMI for Manual configuration",
        "str", 1U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(vehicleType8Id, "VehicleType8", "Vehicle type 8 to be used in DMI for Manual configuration",
        "str", 1U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(vehicleType9Id, "VehicleType9", "Vehicle type 9 to be used in DMI for Manual configuration",
        "str", 1U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(vehicleType10Id, "VehicleType10", "Vehicle type 10 to be used in DMI for Manual configuration",
        "str", 1U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(vehicleType11Id, "VehicleType11", "Vehicle type 11 to be used in DMI for Manual configuration",
        "str", 1U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(vehicleType12Id, "VehicleType12", "Vehicle type 12 to be used in DMI for Manual configuration",
        "str", 1U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(vehicleType13Id, "VehicleType13", "Vehicle type 13 to be used in DMI for Manual configuration",
        "str", 1U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(vehicleType14Id, "VehicleType14", "Vehicle type 14 to be used in DMI for Manual configuration",
        "str", 1U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(vehicleType15Id, "VehicleType15", "Vehicle type 15 to be used in DMI for Manual configuration",
        "str", 1U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(vehicleType16Id, "VehicleType16", "Vehicle type 16 to be used in DMI for Manual configuration",
        "str", 1U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(vehicleType17Id, "VehicleType17", "Vehicle type 17 to be used in DMI for Manual configuration",
        "str", 1U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(vehicleType18Id, "VehicleType18", "Vehicle type 18 to be used in DMI for Manual configuration",
        "str", 1U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(vehicleType19Id, "VehicleType19", "Vehicle type 19 to be used in DMI for Manual configuration",
        "str", 1U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(vehicleType20Id, "VehicleType20", "Vehicle type 20 to be used in DMI for Manual configuration",
        "str", 1U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addUint8ConfigItem(maxSupvGradTargId, "MaxSupvGradTarg", "Maximum number of supervised gradient target",
        "nr", 1U, static_cast<uint8_t>(maxNrSupvGradTargets), 4U, configFileType);
      addUint32ConfigItem(minSupvGradTargDstId, "MinSupvGradTargDist", "Minimum distance for the next supervised gradient target",
        "cm", 1000U, 100000U, 5000U, configFileType);
      addUint8ConfigItem(tractionCutOffDelayId, "TractionCutOffDelay", "Delay time for traction cut-off for brake curve calculation",
        "0.1 s", 0U, 250U, 10U, configFileType);
      addUint8ConfigItem(radioTimeOutYardModeId, "RadioTimeOutYardMode",
        "Time from when the connection is lost until AOS is allowed to enter Yard mode", "s", 0U, 255U, 120U, configFileCommon);
      addUint16ConfigItem(upperSpeedSbDecId, "UpperSpeedSbDec", "Upper Speed boundary value used during supervision of applied SB",
        "cm/s", 0U, 3500U, 0U, configFileType);
      addUint16ConfigItem(lowerSpeedSbDecId, "LowerSpeedSbDec", "Lower Speed boundary value used during supervision of applied SB",
        "cm/s", 0U, 450U, 0U, configFileType);
      addUint16ConfigItem(upperSpeedEbDecId, "UpperSpeedEbDec", "Upper Speed boundary value used during supervision of applied EB",
        "cm/s", 0U, 3500U, 0U, configFileType);
      addUint16ConfigItem(lowerSpeedEbDecId, "LowerSpeedEbDec", "Lower Speed boundary value used during supervision of applied EB",
        "cm/s", 0U, 450U, 0U, configFileType);
      addUint16ConfigItem(timeSbv0Id, "TimeSbv0", "The supervision of an applied service brake requires zero speed within this time.",
        "ms", 0U, 60000U, 60000U, configFileType);
      addUint16ConfigItem(timeEbv0Id, "TimeEbv0", "The supervision of an applied emergency brake requires zero speed within this time.",
        "ms", 0U, 60000U, 60000U, configFileType);
      addUint16ConfigItem(maxLowDecInRowSBId, "MaxLowDecInRowSb",
        "Max Nr bad readings in a row allowed when present acceleration is lesser than expected SB deceleration.",
        "nr", 0U, 500U, 15U, configFileCommon);
      addUint16ConfigItem(maxLowDecAccumSBId, "MaxLowDecAccumSb",
        "Max Nr bad readings accumulative allowed when present acceleration is lesser than expected SB deceleration.",
        "nr", 0U, 500U, 30U, configFileCommon);
      addUint16ConfigItem(maxLowDecInRowEBId, "MaxLowDecInRowEb",
        "Max Nr bad readings in a row allowed when present acceleration is lesser than expected EB deceleration.",
        "nr", 0U, 500U, 15U, configFileCommon);
      addUint16ConfigItem(maxLowDecAccumEBId, "MaxLowDecAccumEb",
        "Max Nr bad readings accumulative allowed when present acceleration is lesser than expected EB deceleration.",
        "nr", 0U, 500U, 30U, configFileCommon);
      addUint8ConfigItem(eventStandstillKeepActiveTimeId, "StandstillActiveTime", "Standstill Event active time",
        "s", 0U, 20U, 1U, configFileCommon);
      addUint16ConfigItem(brakeType1ParamT1SBId, "BrakeType1ParamT1Sb", "Brake System Type 1 Param T1 (Service Brake)",
        "ms", 0U, 65000U, 6000U, configFileType);
      addUint16ConfigItem(brakeType1ParamT2SBId, "BrakeType1ParamT2Sb", "Brake System Type 1 Param T2 (Service Brake)",
        "ms/m", 0U, 3000U, 18U, configFileType);
      addUint16ConfigItem(brakeType1ParamT3SBId, "BrakeType1ParamT3Sb", "Brake System Type 1 Param T3 (Service Brake)",
        "탎/m2", 0U, 65000U, 0U, configFileType);
      addUint16ConfigItem(brakeType2ParamT1SBId, "BrakeType2ParamT1Sb", "Brake System Type 2 Param T1 (Service Brake)",
        "ms", 0U, 65000U, 10000U, configFileType);
      addUint16ConfigItem(brakeType2ParamT2SBId, "BrakeType2ParamT2Sb", "Brake System Type 2 Param T2 (Service Brake)",
        "ms/m", 0U, 3000U, 0U, configFileType);
      addUint16ConfigItem(brakeType2ParamT3SBId, "BrakeType2ParamT3Sb", "Brake System Type 2 Param T3 (Service Brake)",
        "탎/m2", 0U, 65000U, 0U, configFileType);
      addUint16ConfigItem(brakeType3ParamT1SBId, "BrakeType3ParamT1Sb", "Brake System Type 3 Param T1 (Service Brake)",
        "ms", 0U, 65000U, 0U, configFileType);
      addUint16ConfigItem(brakeType3ParamT2SBId, "BrakeType3ParamT2Sb", "Brake System Type 3 Param T2 (Service Brake)",
        "ms/m", 0U, 3000U, 0U, configFileType);
      addUint16ConfigItem(brakeType3ParamT3SBId, "BrakeType3ParamT3Sb", "Brake System Type 3 Param T3 (Service Brake)",
        "탎/m2", 0U, 65000U, 0U, configFileType);
      addUint16ConfigItem(defaultBrakeDelayId, "DefaultBrakeDelay", "Default brake delay",
        "ms", 0U, 65500U, 21000U, configFileType);
      addUint16ConfigItem(defaultBrakeabilityId, "DefaultBrakeability", "Default brakeability",
        "cm/s2", 0U, 400U, 25U, configFileType);
      addUint8ConfigItem(ebMarginAddedId, "EbMarginAdded", "Margin added to supervised SB distance before EB applied",
        "%", 30U, 200U, 100U, configFileType);
      addUint8ConfigItem(timsManualConfTimeId, "TimsManualConfTime", "Time for performing manual integrity confirmation",
        "s", 0U, 10U, 10U, configFileCommon);
      addUint8ConfigItem(timsSensorTimeDiffId, "TimsSensorTimeDiff",
        "Clock accuracy margin between AOS and TIMS sensor; To compensate for possible inaccuracy of the sensors", "s", 0U, 10U, 3U, configFileCommon);
      addUint16ConfigItem(ebPrPropagationSpeedId, "EbPrPropagationSpeed", "EB pressure propagation speed",
        "cm/s", 300U, 30000U, 20000U, configFileType);
      addUint16ConfigItem(releaseSpeedId, "ReleaseSpeed", "Minimum supervised speed when close to the primary target",
        "cm/s", 28U, 139U, 54U, configFileCommon);
      addUint8ConfigItem(departureWarningId, "DepartureWarning", "Time from first warning sound of departure warning until train movement allowed",
        "s", 5U, 10U, 5U, configFileCommon);
      addUint8ConfigItem(vNomMarginId, "VNomMargin", "Margin for allowed variance in difference in vMax/vMin and vNom",
        "%", 0U, 100U, 25U, configFileType);
      addUint8ConfigItem(timeSyncServerVersion1Id, "TimeSyncSrvVersion1", "1st number of the version (or 255 if this number isn't used)",
        "nr", 0U, 255U, 7U, configFileCommon);
      addUint8ConfigItem(timeSyncServerVersion2Id, "TimeSyncSrvVersion2", "2nd number of the version (or 255 if this number isn't used)",
        "nr", 0U, 255U, 1U, configFileCommon);
      addUint8ConfigItem(timeSyncServerVersion3Id, "TimeSyncSrvVersion3", "3rd number of the version (or 255 if this number isn't used)",
        "nr", 0U, 255U, 1U, configFileCommon);
      addUint8ConfigItem(timeSyncServerVersion4Id, "TimeSyncSrvVersion4", "4th number of the version (or 255 if this number isn't used)",
        "nr", 0U, 255U, 255U, configFileCommon);
      addUint16ConfigItem(brakeType1ParamT1EBId, "BrakeType1ParamT1Eb", "Brake System Type 1 Param T1 (Emergency Brake)",
        "ms", 0U, 65000U, 6000U, configFileType);
      addUint16ConfigItem(brakeType1ParamT2EBId, "BrakeType1ParamT2Eb", "Brake System Type 1 Param T2 (Emergency Brake)",
        "ms/m", 0U, 3000U, 5U, configFileType);
      addUint16ConfigItem(brakeType1ParamT3EBId, "BrakeType1ParamT3Eb", "Brake System Type 1 Param T3 (Emergency Brake)",
        "탎/m2", 0U, 65000U, 0U, configFileType);
      addUint16ConfigItem(brakeType2ParamT1EBId, "BrakeType2ParamT1Eb", "Brake System Type 2 Param T1 (Emergency Brake)",
        "ms", 0U, 65000U, 10000U, configFileType);
      addUint16ConfigItem(brakeType2ParamT2EBId, "BrakeType2ParamT2Eb", "Brake System Type 2 Param T2 (Emergency Brake)",
        "ms/m", 0U, 3000U, 0U, configFileType);
      addUint16ConfigItem(brakeType2ParamT3EBId, "BrakeType2ParamT3Eb", "Brake System Type 2 Param T3 (Emergency Brake)",
        "탎/m2", 0U, 65000U, 0U, configFileType);
      addUint16ConfigItem(brakeType3ParamT1EBId, "BrakeType3ParamT1Eb", "Brake System Type 3 Param T1 (Emergency Brake)",
        "ms", 0U, 65000U, 0U, configFileType);
      addUint16ConfigItem(brakeType3ParamT2EBId, "BrakeType3ParamT2Eb", "Brake System Type 3 Param T2 (Emergency Brake)",
        "ms/m", 0U, 3000U, 0U, configFileType);
      addUint16ConfigItem(brakeType3ParamT3EBId, "BrakeType3ParamT3Eb", "Brake System Type 3 Param T3 (Emergency Brake)",
        "탎/m2", 0U, 65000U, 0U, configFileType);
    }

    // Then call the base class init()
    if (ATC::AbstractConfigBase::init())
    {
      //set Global Name to the corresponding Configuration parameter
      setGlobalNameToConfigParameter(balSearchSpeedId, "V_BALISESEARCH");
      setGlobalNameToConfigParameter(balSearchDistanceId, "D_BALISESEARCH");
      setGlobalNameToConfigParameter(secBalSearchDistanceId, "D_BALISESEARCH2");
      setGlobalNameToConfigParameter(yardSpeedId, "V_YARD");
      setGlobalNameToConfigParameter(rollAwayMarginId, "D_ROLLAWAY");
      setGlobalNameToConfigParameter(revSupMarginNormId, "D_REVERSING");
      setGlobalNameToConfigParameter(revSupMarginShuntId, "D_REVERSINGSHUNTING");
      setGlobalNameToConfigParameter(radioTimeOutId, "T_RADIOTIMEOUT");
      setGlobalNameToConfigParameter(fwToSwDelayId, "T_FIRSTWARNING");
      setGlobalNameToConfigParameter(swToSBDelayId, "T_SECONDWARNING");
      setGlobalNameToConfigParameter(minSpeedMarginSBId, "V_SBOVERSPEEDMIN");
      setGlobalNameToConfigParameter(minSpeedMarginEBId, "V_EBOVERSPEEDMIN");
      setGlobalNameToConfigParameter(maxSpeedMarginSBId, "V_SBOVERSPEEDMAX");
      setGlobalNameToConfigParameter(maxSpeedMarginEBId, "V_EBOVERSPEEDMAX");
      setGlobalNameToConfigParameter(speedMarginSBId, "Pm_SBOVERSPEED");
      setGlobalNameToConfigParameter(speedMarginEBId, "Pm_EBOVERSPEED");
      setGlobalNameToConfigParameter(vehicleType1Id, "S_VEHICLETYPE1");
      setGlobalNameToConfigParameter(vehicleType2Id, "S_VEHICLETYPE2");
      setGlobalNameToConfigParameter(vehicleType3Id, "S_VEHICLETYPE3");
      setGlobalNameToConfigParameter(vehicleType4Id, "S_VEHICLETYPE4");
      setGlobalNameToConfigParameter(vehicleType5Id, "S_VEHICLETYPE5");
      setGlobalNameToConfigParameter(vehicleType6Id, "S_VEHICLETYPE6");
      setGlobalNameToConfigParameter(vehicleType7Id, "S_VEHICLETYPE7");
      setGlobalNameToConfigParameter(vehicleType8Id, "S_VEHICLETYPE8");
      setGlobalNameToConfigParameter(vehicleType9Id, "S_VEHICLETYPE9");
      setGlobalNameToConfigParameter(vehicleType10Id, "S_VEHICLETYPE10");
      setGlobalNameToConfigParameter(vehicleType11Id, "S_VEHICLETYPE11");
      setGlobalNameToConfigParameter(vehicleType12Id, "S_VEHICLETYPE12");
      setGlobalNameToConfigParameter(vehicleType13Id, "S_VEHICLETYPE13");
      setGlobalNameToConfigParameter(vehicleType14Id, "S_VEHICLETYPE14");
      setGlobalNameToConfigParameter(vehicleType15Id, "S_VEHICLETYPE15");
      setGlobalNameToConfigParameter(vehicleType16Id, "S_VEHICLETYPE16");
      setGlobalNameToConfigParameter(vehicleType17Id, "S_VEHICLETYPE17");
      setGlobalNameToConfigParameter(vehicleType18Id, "S_VEHICLETYPE18");
      setGlobalNameToConfigParameter(vehicleType19Id, "S_VEHICLETYPE19");
      setGlobalNameToConfigParameter(vehicleType20Id, "S_VEHICLETYPE20");
      setGlobalNameToConfigParameter(radioTimeOutSbId, "T_RADIOTIMEOUTBRAKE");
      setGlobalNameToConfigParameter(balWindowPermilId, "Pm_ODOINACCURACY");
      setGlobalNameToConfigParameter(ebMarginAddedId, "P_EBMARGINADDED");
      setGlobalNameToConfigParameter(departureWarningId, "T_DEPARTUREWARNING");
      setGlobalNameToConfigParameter(releaseSpeedId, "V_MINSUPERVISEDSPEED");

      retValue = true;
    }

    if (!coreInitDone)
    {
      initCrossCompare();

      coreInitDone = true;
    }

    return retValue;
  }

  /******************************************************************************
  * corePtr
  ******************************************************************************/
  AbstractConfig* AbstractConfig::corePtr(void)
  {
    AbstractConfig* ptr = ATC::dynamicCast<AbstractConfigBase*, AbstractConfig*>(ATC::AbstractConfigBase::basePtr(), __FILE__, __LINE__); 

    return ptr;
  }

  /******************************************************************************
  * getBalNidC
  ******************************************************************************/
  uint16_t AbstractConfig::getBalNidC() const
  {
    uint16_t val;

    if (!getConfig(balNidCId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getBalNidC() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getRadioId
  ******************************************************************************/
  uint16_t AbstractConfig::getRadioId() const
  {
    uint16_t val;

    if (!getConfig(radioIdId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getRadioId() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getSiteId
  ******************************************************************************/
  uint8_t AbstractConfig::getSiteId() const
  {
    uint8_t val;

    if (!getConfig(siteIdId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getSiteId() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getRadioTimeOut
  ******************************************************************************/
  uint8_t AbstractConfig::getRadioTimeOut() const
  {
    uint8_t val;

    if (!getConfig(radioTimeOutId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getRadioTimeOut() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getRadioTimeOutSb
  ******************************************************************************/
  uint8_t AbstractConfig::getRadioTimeOutSb() const
  {
    uint8_t val;

    if (!getConfig(radioTimeOutSbId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getRadioTimeOutSb() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getBalSearchDistance
  ******************************************************************************/
  uint16_t AbstractConfig::getBalSearchDistance() const
  {
    uint16_t val;

    if (!getConfig(balSearchDistanceId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getBalSearchDistance() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getSecondBaliseSearchDistance
  ******************************************************************************/
  uint16_t AbstractConfig::getSecondBaliseSearchDistance() const
  {
    uint16_t val;

    if (!getConfig(secBalSearchDistanceId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getSecondBaliseSearchDistance() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getBalSearchSpeed
  ******************************************************************************/
  uint16_t AbstractConfig::getBalSearchSpeed() const
  {
    uint16_t val;

    if (!getConfig(balSearchSpeedId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getBalSearchSpeed() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getBalWindowMin
  ******************************************************************************/
  uint16_t AbstractConfig::getBalWindowMin() const
  {
    uint16_t val;

    if (!getConfig(balWindowMinId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getBalWindowMin() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getBalWindowPermil
  ******************************************************************************/
  uint8_t AbstractConfig::getBalWindowPermil() const
  {
    uint8_t val;

    if (!getConfig(balWindowPermilId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getBalWindowPermil() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getYardSpeed
  ******************************************************************************/
  uint16_t AbstractConfig::getYardSpeed() const
  {
    uint16_t val;

    if (!getConfig(yardSpeedId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getYardSpeed() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getAtoEnable
  ******************************************************************************/
  bool AbstractConfig::getAtoEnable() const
  {
    bool val;

    if (!getConfig(atoEnableId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getAtoEnable() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getRevSupMargin
  ******************************************************************************/
  uint16_t AbstractConfig::getRevSupMargin() const
  {
    uint16_t val;

    if (!getConfig(revSupMarginNormId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getRevSupMargin() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getRevSupMargForShuntRoute
  ******************************************************************************/
  uint16_t AbstractConfig::getRevSupMargForShuntRoute() const
  {
    uint16_t val;

    if (!getConfig(revSupMarginShuntId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getRevSupMargForShuntRoute() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getRevSupMargForFreeRolling
  ******************************************************************************/
  uint16_t AbstractConfig::getRevSupMargForFreeRolling() const
  {
    uint16_t val;

    if (!getConfig(revSupMarginFreeRollingId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getRevSupMargForFreeRolling() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getRollAwayMargin
  ******************************************************************************/
  uint16_t AbstractConfig::getRollAwayMargin() const
  {
    uint16_t val;

    if (!getConfig(rollAwayMarginId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getRollAwayMargin() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getTachoPulsesPer10Revolutions1
  ******************************************************************************/
  uint16_t AbstractConfig::getTachoPulsesPer10Revolutions1() const
  {
    uint16_t val;

    if (!getConfig(tachoPulsesPer10Rev1Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getWheel1TachoPulsesPer10Revolutions() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getTachoPulsesPer10Revolutions2
  ******************************************************************************/
  uint16_t AbstractConfig::getTachoPulsesPer10Revolutions2() const
  {
    uint16_t val;

    if (!getConfig(tachoPulsesPer10Rev2Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getWheel2TachoPulsesPer10Revolutions() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getWheelSize1
  ******************************************************************************/
  uint16_t AbstractConfig::getWheelSize1() const
  {
    uint16_t val;

    if (!getConfig(wheelSize1Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getWheelSize1() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getWheelSize2
  ******************************************************************************/
  uint16_t AbstractConfig::getWheelSize2() const
  {
    uint16_t val;

    if (!getConfig(wheelSize2Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getWheelSize2() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getBalAntennaPosEnd
  ******************************************************************************/
  uint16_t AbstractConfig::getBalAntennaPosEnd() const
  {
    uint16_t val;

    if (!getConfig(balAntennaPosEndId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getBalAntennaPosEnd() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getBalAntennaPosFront
  ******************************************************************************/
  uint16_t AbstractConfig::getBalAntennaPosFront() const
  {
    uint16_t val;

    if (!getConfig(balAntennaPosFrontId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getBalAntennaPosFront() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getMaxWheelSizeError
  ******************************************************************************/
  uint8_t AbstractConfig::getMaxWheelSizeError() const
  {
    uint8_t val;

    if (!getConfig(maxWheelSizeErrorId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getMaxWheelSizeError() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getMinSpeedMarginWarn
  ******************************************************************************/
  uint16_t AbstractConfig::getMinSpeedMarginWarn() const
  {
    uint16_t val;

    if (!getConfig(minSpeedMarginWarnId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getMinSpeedMarginWarn() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getMinSpeedMarginSB
  ******************************************************************************/
  uint16_t AbstractConfig::getMinSpeedMarginSB() const
  {
    uint16_t val;

    if (!getConfig(minSpeedMarginSBId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getMinSpeedMarginSB() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getMinSpeedMarginEB
  ******************************************************************************/
  uint16_t AbstractConfig::getMinSpeedMarginEB() const
  {
    uint16_t val;

    if (!getConfig(minSpeedMarginEBId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getMinSpeedMarginEB() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getMaxSpeedMarginWarn
  ******************************************************************************/
  uint16_t AbstractConfig::getMaxSpeedMarginWarn() const
  {
    uint16_t val;

    if (!getConfig(maxSpeedMarginWarnId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getMaxSpeedMarginWarn() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getMaxSpeedMarginSB
  ******************************************************************************/
  uint16_t AbstractConfig::getMaxSpeedMarginSB() const
  {
    uint16_t val;

    if (!getConfig(maxSpeedMarginSBId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getMaxSpeedMarginSB() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getMaxSpeedMarginEB
  ******************************************************************************/
  uint16_t AbstractConfig::getMaxSpeedMarginEB() const
  {
    uint16_t val;

    if (!getConfig(maxSpeedMarginEBId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getMaxSpeedMarginEB() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getSpeedMarginWarn
  ******************************************************************************/
  uint16_t AbstractConfig::getSpeedMarginWarn() const
  {
    uint16_t val;

    if (!getConfig(speedMarginWarnId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getSpeedMarginWarn() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getSpeedMarginSB
  ******************************************************************************/
  uint16_t AbstractConfig::getSpeedMarginSB() const
  {
    uint16_t val;

    if (!getConfig(speedMarginSBId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getSpeedMarginSB() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getSpeedMarginEB
  ******************************************************************************/
  uint16_t AbstractConfig::getSpeedMarginEB() const
  {
    uint16_t val;

    if (!getConfig(speedMarginEBId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getSpeedMarginEB() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getFirstToSecondWarnCurveDelay
  ******************************************************************************/
  uint16_t AbstractConfig::getFirstToSecondWarnCurveDelay() const
  {
    uint16_t val;

    if (!getConfig(fwToSwDelayId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getDelay1stTo2ndWarn() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getSecondToSBCurveDelay
  ******************************************************************************/
  uint16_t AbstractConfig::getSecondToSBCurveDelay() const
  {
    uint16_t val;

    if (!getConfig(swToSBDelayId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getWarnToSbDelay2nd() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getSbToEbDelay
  ******************************************************************************/
  uint16_t AbstractConfig::getSbToEbDelay() const
  {
    uint16_t val;

    if (!getConfig(sbToEbDelayId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getSbToEbDelay() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getEffectiveEbDelay
  ******************************************************************************/
  uint16_t AbstractConfig::getEffectiveEbDelay() const
  {
    uint16_t val;

    if (!getConfig(effectiveEbDelayId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getEffectiveEbDelay() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getEventKeepActiveTime
  ******************************************************************************/
  uint8_t AbstractConfig::getEventKeepActiveTime() const
  {
    uint8_t val;

    if (!getConfig(eventKeepActiveTimeId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getEventKeepActiveTime() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getStandstillEventKeepActiveTime
  ******************************************************************************/
  uint8_t AbstractConfig::getStandstillEventKeepActiveTime() const
  {
     uint8_t val;

     if (!getConfig(eventStandstillKeepActiveTimeId, val))
     {
        reportConfigError(__FILE__, __LINE__, "getStandstillEventKeepActiveTime() failed, returning invalid value");
     }

     return val;
  }

  /******************************************************************************
  * getTrainCfgTicTimeout
  ******************************************************************************/
  uint16_t AbstractConfig::getTrainCfgTicTimeout() const
  {
    uint16_t val;

    if (!getConfig(trainCfgTicTimeoutId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getTrainCfgTicTimeout() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getAnalyzerIFPort
  ******************************************************************************/
  uint16_t AbstractConfig::getAnalyzerIFPort() const
  {
    uint16_t val;

    if (!getConfig(analyzerIFPortId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getAnalyzerIFPort() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getSendCycleAIF
  ******************************************************************************/
  uint8_t AbstractConfig::getSendCycleAIF() const
  {
    uint8_t val;

    if (!getConfig(sendCycleAIFId, val))
    {
      reportConfigError(__FILE__, __LINE__, "sendCycleForAOSAnalyzer() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getCODSensorConfiguration
  ******************************************************************************/
  uint8_t AbstractConfig::getCODSensorConfiguration() const
  {
    uint8_t val;

    if (!getConfig(codSensorConfigId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getCODSensorConfiguration() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getTractionControlValue
  ******************************************************************************/
  uint8_t AbstractConfig::getTractionControlValue() const
  {
    uint8_t val;

    if (!getConfig(tractionControlId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getTractionControlValue() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getTachoDirection1Value
  ******************************************************************************/
  uint8_t AbstractConfig::getTacho1Direction() const
  {
    uint8_t val;

    if (!getConfig(tacho1DirectionId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getTachoDirection1Value() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getTachoDirection2Value
  ******************************************************************************/
  uint8_t AbstractConfig::getTacho2Direction() const
  {
    uint8_t val;

    if (!getConfig(tacho2DirectionId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getTachoDirection2Value() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getMinDopplerSpeed
  ******************************************************************************/
  uint16_t AbstractConfig::getMinDopplerSpeed() const
  {
    uint16_t val;

    if (!getConfig(minDopplerSpeedId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getMinDopplerSpeed() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getMaxDopplerSpeed
  ******************************************************************************/
  uint16_t AbstractConfig::getMaxDopplerSpeed() const
  {
    uint16_t val;

    if (!getConfig(maxDopplerSpeedId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getMaxDopplerSpeed() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getMaxDopplerAccel
  ******************************************************************************/
  uint16_t AbstractConfig::getMaxDopplerAccel() const
  {
    uint16_t val;

    if (!getConfig(maxDopplerAccelId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getMaxDopplerAccel() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getDopplerPulsesPerKm
  ******************************************************************************/
  uint32_t AbstractConfig::getDopplerPulsesPerKm() const
  {
    uint32_t val;

    if (!getConfig(dopplerPulsesPerKilometerId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getDopplerPulsesPerKm() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getBTMTestNotify
  ******************************************************************************/
  uint16_t AbstractConfig::getBTMTestNotify() const
  {
    uint16_t val;

    if (!getConfig(btmTestNotifyId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getBTMTestNotify() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getBTMTestMandatory
  ******************************************************************************/
  uint16_t AbstractConfig::getBTMTestMandatory() const
  {
    uint16_t val;

    if (!getConfig(btmTestMandatoryId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getBTMTestMandatory() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getLastBTMTestTime
  ******************************************************************************/
  uint32_t AbstractConfig::getLastBTMTestTime() const
  {
    uint32_t val;

    if (!getConfig(lastBTMTestTimeId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getLastBTMTest() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getBrakeTestNotifyTime
  ******************************************************************************/
  uint16_t AbstractConfig::getBrakeTestNotifyTime() const
  {
    uint16_t val;

    if (!getConfig(brakeTestNotifyId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getBrakeTestNotifyTime() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getBrakeTestMandatoryTime
  ******************************************************************************/
  uint16_t AbstractConfig::getBrakeTestMandatoryTime() const
  {
    uint16_t val;

    if (!getConfig(brakeTestMandatoryId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getBrakeTestMandatoryTime() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getLastBrakeTestTime
  ******************************************************************************/
  uint32_t AbstractConfig::getLastBrakeTestTime() const
  {
    uint32_t val;

    if (!getConfig(lastBrakeTestTimeId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getLastBrakeTestTime() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getBrakeTestExecTime
  ******************************************************************************/
  uint16_t AbstractConfig::getBrakeTestExecTime() const
  {
    uint16_t val;

    if (!getConfig(brakeTestExecTimeId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getBrakeTestExecTime() failed, returning invalid value");
    }

    return val;
  }
  /******************************************************************************
  * getDopplerPulsePrecision
  ******************************************************************************/
  uint8_t AbstractConfig::getDopplerPulsePrecision() const
  {
    uint8_t val;

    if (!getConfig(dopplerPulsePrecisionId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getDopplerPulsePrecision() failed, returning invalid value");
    }

    return val;
  }


  /******************************************************************************
  * getSbExpectedDecLimit
  ******************************************************************************/
  uint8_t AbstractConfig::getSbExpectedDecLimit() const
  {
    uint8_t val;

    if (!getConfig(sbExpectedDecLimitId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getSbExpectedDecLimit() failed, returning invalid value");
    }
    return val;
  }


  /******************************************************************************
  * getEbExpectedDecLimit
  ******************************************************************************/
  uint8_t AbstractConfig::getEbExpectedDecLimit() const
  {
    uint8_t val;

    if (!getConfig(ebExpectedDecLimitId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getEbExpectedDecLimit() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getUpperSpeedSbDec
  ******************************************************************************/
  uint16_t AbstractConfig::getUpperSpeedSbDec() const
  {
    uint16_t val;

    if (!getConfig(upperSpeedSbDecId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getUpperSpeedSbDec() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getLowerSpeedSbDec
  ******************************************************************************/
  uint16_t AbstractConfig::getLowerSpeedSbDec() const
  {
    uint16_t val;

    if (!getConfig(lowerSpeedSbDecId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getLowerSpeedSbDec() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getUpperSpeedEbDec
  ******************************************************************************/
  uint16_t AbstractConfig::getUpperSpeedEbDec() const
  {
    uint16_t val;

    if (!getConfig(upperSpeedEbDecId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getUpperSpeedEbDec() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getLowerSpeedSbDec
  ******************************************************************************/
  uint16_t AbstractConfig::getLowerSpeedEbDec() const
  {
    uint16_t val;

    if (!getConfig(lowerSpeedSbDecId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getLowerSpeedSbDec() failed, returning invalid value");
    }

    return val;
  }


  /******************************************************************************
  * getTimeSbv0
  ******************************************************************************/
  uint16_t AbstractConfig::getTimeSbv0() const
  {
    uint16_t val;

    if (!getConfig(timeSbv0Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getTimeSbv0() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getTimeEbv0
  ******************************************************************************/
  uint16_t AbstractConfig::getTimeEbv0() const
  {
    uint16_t val;

    if (!getConfig(timeEbv0Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getTimeEbv0() failed, returning invalid value");
    }

    return val;
  }
    
  /******************************************************************************
  * getMaxLowDecInRowSBId
  ******************************************************************************/
  uint16_t AbstractConfig::getMaxLowDecInRowSB() const
  {
    uint16_t val;

    if (!getConfig(maxLowDecInRowSBId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getMaxLowDecInRowSBId() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getMaxLowDecAccumSBId
  ******************************************************************************/
  uint16_t AbstractConfig::getMaxLowDecAccumSB() const
  {
    uint16_t val;

    if (!getConfig(maxLowDecAccumSBId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getMaxLowDecAccumSBId() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getMaxLowDecInRowEBId
  ******************************************************************************/
  uint16_t AbstractConfig::getMaxLowDecInRowEB() const
  {
    uint16_t val;

    if (!getConfig(maxLowDecInRowEBId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getMaxLowDecInRowEBId() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getMaxLowDecAccumEBId
  ******************************************************************************/
  uint16_t AbstractConfig::getMaxLowDecAccumEB() const
  {
    uint16_t val;

    if (!getConfig(maxLowDecAccumEBId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getMaxLowDecAccumEBId() failed, returning invalid value");
    }

    return val;
  }
  
  /******************************************************************************
  * getLocoType
  ******************************************************************************/
  uint8_t AbstractConfig::getLocoType() const
  {
    uint8_t val;

    if (!getConfig(locoTypeId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getLocoType() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getCabinConfiguration
  ******************************************************************************/
  uint8_t AbstractConfig::getCabinConfiguration() const
  {
    uint8_t val;

    if (!getConfig(cabinConfigurationId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getCabinConfiguration() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getInternalRelaysTimeout
  ******************************************************************************/
  uint16_t AbstractConfig::getInternalRelaysTimeout() const
  {
    uint16_t val;

    if (!getConfig(intRelaysTimeoutId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getInternalRelaysTimeout() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
    * getESAInputAvail
    ******************************************************************************/
  bool AbstractConfig::getESAInputAvail() const
  {
    bool val;

    if (!getConfig(esaInputAvailId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getESAInputAvail() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getBrakeTestReason
  ******************************************************************************/
  uint32_t AbstractConfig::getBrakeTestReason() const
  {
    uint32_t val;

    if (!getConfig(brakeTestReasonId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getBrakeTestReason() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getSbAvailable
  ******************************************************************************/
  bool AbstractConfig::getSbAvailable() const
  {
    bool val;

    if (!getConfig(sbAvailId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getSbAvailable() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * setBrakeTestReason
  ******************************************************************************/
  bool AbstractConfig::setBrakeTestReason(const BrakeTestReasonType brakeTestReason)
  {
    bool retValue = true; // Return true unless the setRunTimeConfigValue fails.
    bool setValue = true;
    
    // To not allow for different components to keep writing different values to NVS 
    // The first written brake test reason value is the valid value.
    if (braketestReasonNone != brakeTestReason)
    {
      // Read and compare the stored value to the given brake test reason
      ATC::BaseConfigItem* item = getConfigItem(brakeTestReasonId);
      if (item != NULL)
      {
        if (configFileRuntime == item->getConfigFile())
        {
          ATC::Uint32ConfigItem* itemUint32 =
            ATC::dynamicCast<ATC::BaseConfigItem*, ATC::Uint32ConfigItem*>(item, __FILE__, __LINE__);
          if (itemUint32 != NULL)
          {
            if (braketestReasonNone != itemUint32->getValue())
            {
              //if the stored value is other than braketestReasonNone we don't overwrite it, 
              //since the existing value is valid.
              setValue = false;
            }
          }
        }
      }
    }

    if (setValue)
    {
      retValue = setRunTimeConfigValue(brakeTestReasonId, static_cast<uint32_t>(brakeTestReason)); //setRunTimeConfigValue only support uint32_t
      if (!retValue)
      {
        reportConfigError(__FILE__, __LINE__, "setLastBTMTestTime() failed, returning invalid value");
      }
    }
    return retValue;
  }

  /******************************************************************************
  * setLastBTMTestTime
  ******************************************************************************/
  bool AbstractConfig::setLastBTMTestTime(const uint32_t timeToStoreInNVS)
  {
    const bool retValue = setRunTimeConfigValue(lastBTMTestTimeId, timeToStoreInNVS);
    if (!retValue)
    {
      reportConfigError(__FILE__, __LINE__, "setLastBTMTestTime() failed, returning invalid value");
    }

    return retValue;
  }

  /******************************************************************************
  * setLastBrakeTestTime
  ******************************************************************************/
  bool AbstractConfig::setLastBrakeTestTime(const uint32_t timeToStoreInNVS)
  {
    const bool retValue = setRunTimeConfigValue(lastBrakeTestTimeId, timeToStoreInNVS);
    if (!retValue)
    {
      reportConfigError(__FILE__, __LINE__, "setLastBrakeTestTime() failed, returning invalid value");
    }

    return retValue;
  }

  /******************************************************************************
  * getMaxTimeAOSRun
  ******************************************************************************/
  uint16_t AbstractConfig::getMaxTimeAOSRun() const
  {
    uint16_t val;

    if (!getConfig(maxTimeAOSRunId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getMaxTimeAOSRun() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getBrakeSystemParameters
  ******************************************************************************/
  uint16_t AbstractConfig::getBrakeParameter(const BrakeSystemType brakeType, const BrakeParameter brakeParameter) const
  {
    ATC::ConfigIdType brakeParamId = 0U;
    uint16_t brakeParamvalue = 0U;

    switch (brakeType)
    {
    case BrakeSystemType1:
    {
      switch (brakeParameter)
      {
        case BrakeParameterA1:
          brakeParamId = brakeType1ParamA1Id;
          break;

        case BrakeParameterA2:
          brakeParamId = brakeType1ParamA2Id;
          break;

        case BrakeParameterA3:
          brakeParamId = brakeType1ParamA3Id;
          break;

        case BrakeParameterB1:
          brakeParamId = brakeType1ParamB1Id;
          break;

        case BrakeParameterB2:
          brakeParamId = brakeType1ParamB2Id;
          break;

        case BrakeParameterB3:
          brakeParamId = brakeType1ParamB3Id;
          break;

        case BrakeParameterV1:
          brakeParamId = brakeType1ParamV1Id;
          break;

        case BrakeParameterV2:
          brakeParamId = brakeType1ParamV2Id;
          break;

        case BrakeParameterT1SB:
          brakeParamId = brakeType1ParamT1SBId;
          break;

        case BrakeParameterT2SB:
          brakeParamId = brakeType1ParamT2SBId;
          break;

        case BrakeParameterT3SB:
          brakeParamId = brakeType1ParamT3SBId;
          break;

        case BrakeParameterT1EB:
          brakeParamId = brakeType1ParamT1EBId;
          break;

        case BrakeParameterT2EB:
          brakeParamId = brakeType1ParamT2EBId;
          break;

        case BrakeParameterT3EB:
          brakeParamId = brakeType1ParamT3EBId;
          break;

        default:
        {
          reportConfigError(__FILE__, __LINE__, "Brake Parameter not defined!");
          break;
        }
      }
      break;
    }

    case BrakeSystemType2:
    {
      switch (brakeParameter)
      {
        case BrakeParameterA1:
          brakeParamId = brakeType2ParamA1Id;
          break;

        case BrakeParameterA2:
          brakeParamId = brakeType2ParamA2Id;
          break;

        case BrakeParameterA3:
          brakeParamId = brakeType2ParamA3Id;
          break;

        case BrakeParameterB1:
          brakeParamId = brakeType2ParamB1Id;
          break;

        case BrakeParameterB2:
          brakeParamId = brakeType2ParamB2Id;
          break;

        case BrakeParameterB3:
          brakeParamId = brakeType2ParamB3Id;
          break;

        case BrakeParameterV1:
          brakeParamId = brakeType2ParamV1Id;
          break;

        case BrakeParameterV2:
          brakeParamId = brakeType2ParamV2Id;
          break;

        case BrakeParameterT1SB:
          brakeParamId = brakeType2ParamT1SBId;
          break;

        case BrakeParameterT2SB:
          brakeParamId = brakeType2ParamT2SBId;
          break;

        case BrakeParameterT3SB:
          brakeParamId = brakeType2ParamT3SBId;
          break;

        case BrakeParameterT1EB:
          brakeParamId = brakeType2ParamT1EBId;
          break;

        case BrakeParameterT2EB:
          brakeParamId = brakeType2ParamT2EBId;
          break;

        case BrakeParameterT3EB:
          brakeParamId = brakeType2ParamT3EBId;
          break;

        default:
          reportConfigError(__FILE__, __LINE__, "Brake Parameter not defined!");
          break;
      }
      break;
    }

    case BrakeSystemType3:
    {
      switch (brakeParameter)
      {
        case BrakeParameterA1:
          brakeParamId = brakeType3ParamA1Id;
          break;

        case BrakeParameterA2:
          brakeParamId = brakeType3ParamA2Id;
          break;

        case BrakeParameterA3:
          brakeParamId = brakeType3ParamA3Id;
          break;

        case BrakeParameterB1:
          brakeParamId = brakeType3ParamB1Id;
          break;

        case BrakeParameterB2:
          brakeParamId = brakeType3ParamB2Id;
          break;

        case BrakeParameterB3:
          brakeParamId = brakeType3ParamB3Id;
          break;

        case BrakeParameterV1:
          brakeParamId = brakeType3ParamV1Id;
          break;

        case BrakeParameterV2:
          brakeParamId = brakeType3ParamV2Id;
          break;

        case BrakeParameterT1SB:
          brakeParamId = brakeType3ParamT1SBId;
          break;

        case BrakeParameterT2SB:
          brakeParamId = brakeType3ParamT2SBId;
          break;

        case BrakeParameterT3SB:
          brakeParamId = brakeType3ParamT3SBId;
          break;

        case BrakeParameterT1EB:
          brakeParamId = brakeType3ParamT1EBId;
          break;

        case BrakeParameterT2EB:
          brakeParamId = brakeType3ParamT2EBId;
          break;

        case BrakeParameterT3EB:
          brakeParamId = brakeType3ParamT3EBId;
          break;

        default:
          reportConfigError(__FILE__, __LINE__, "Brake Parameter not defined!");
          break;
      }
      break;
    }

    case BrakeSystemTypeUndefined:
    default:
      reportConfigError(__FILE__, __LINE__, "Brake System not defined!");
      break;
    }

    if (brakeParamId > 0U)
    {
      //Get the Brake Parameter
      if (!getConfig(brakeParamId, brakeParamvalue))
      {
        reportConfigError(__FILE__, __LINE__, "Brake System Parameter returning invalid value");
      }
    }

    return brakeParamvalue;
  }


  /******************************************************************************
  * getBrakeLambdaMin
  ******************************************************************************/
  uint8_t AbstractConfig::getBrakeLambdaMin(const BrakeSystemType brakeType) const
  {
    ATC::ConfigIdType lamnbdaMinId = 0U;
    uint8_t lambdaMinValue = 0U;

    switch (brakeType)
    {
    case BrakeSystemType1:
    {
      lamnbdaMinId = brakeType1LambdaMinId;
      break;
    }

    case BrakeSystemType2:
    {
      lamnbdaMinId = brakeType2LambdaMinId;
      break;
    }

    case BrakeSystemType3:
    {
      lamnbdaMinId = brakeType3LambdaMinId;
      break;
    }

    case BrakeSystemTypeUndefined:
    default:
    {
      reportConfigError(__FILE__, __LINE__, "Brake System not defined!");
    }
    }

    //Get the Brake System Lambda Minimum
    if (!getConfig(lamnbdaMinId, lambdaMinValue))
    {
      reportConfigError(__FILE__, __LINE__, "getBrakeLambdaMin() failed, returning invalid value");
    }

    return lambdaMinValue;
  }

  /******************************************************************************
  * getBrakeLambdaMax
  ******************************************************************************/
  uint8_t AbstractConfig::getBrakeLambdaMax(const BrakeSystemType brakeType) const
  {
    ATC::ConfigIdType lamnbdaMaxId = 0U;
    uint8_t lambdaMaxValue = 0U;

    switch (brakeType)
    {
    case BrakeSystemType1:
    {
      lamnbdaMaxId = brakeType1LambdaMaxId;
      break;
    }

    case BrakeSystemType2:
    {
      lamnbdaMaxId = brakeType2LambdaMaxId;
      break;
    }

    case BrakeSystemType3:
    {
      lamnbdaMaxId = brakeType3LambdaMaxId;
      break;
    }

    case BrakeSystemTypeUndefined:
    default:
    {
      reportConfigError(__FILE__, __LINE__, "Brake System not defined!");
    }
    }

    //Get the Brake System Lambda Minimum
    if (!getConfig(lamnbdaMaxId, lambdaMaxValue))
    {
      reportConfigError(__FILE__, __LINE__, "getBrakeLambdaMax() failed, returning invalid value");
    }

    return lambdaMaxValue;
  }


  /******************************************************************************
  * getMaxAcceleration
  ******************************************************************************/
  uint16_t AbstractConfig::getMaxAcceleration() const
  {
    uint16_t val;

    if (!getConfig(maxAccelerationId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getMaxAcceleration() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getMaxDeceleration
  ******************************************************************************/
  uint16_t AbstractConfig::getMaxDeceleration() const
  {
    uint16_t val;

    if (!getConfig(maxDecelerationId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getMaxDeceleration() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getVehicleType1
  ******************************************************************************/
  const char_t * AbstractConfig::getVehicleType1() const
  {
    const char_t *val;
    if (!getConfig(vehicleType1Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getVehicleType1() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getVehicleType2
  ******************************************************************************/
  const char_t * AbstractConfig::getVehicleType2() const
  {
    const char_t *val;
    if (!getConfig(vehicleType2Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getVehicleType2() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getVehicleType3
  ******************************************************************************/
  const char_t * AbstractConfig::getVehicleType3() const
  {
    const char_t *val;
    if (!getConfig(vehicleType3Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getVehicleType3() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getVehicleType4
  ******************************************************************************/
  const char_t * AbstractConfig::getVehicleType4() const
  {
    const char_t *val;
    if (!getConfig(vehicleType4Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getVehicleType4() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getVehicleType5
  ******************************************************************************/
  const char_t * AbstractConfig::getVehicleType5() const
  {
    const char_t *val;
    if (!getConfig(vehicleType5Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getVehicleType5() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getVehicleType6
  ******************************************************************************/
  const char_t * AbstractConfig::getVehicleType6() const
  {
    const char_t *val;
    if (!getConfig(vehicleType6Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getVehicleType6() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getVehicleType7
  ******************************************************************************/
  const char_t * AbstractConfig::getVehicleType7() const
  {
    const char_t *val;
    if (!getConfig(vehicleType7Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getVehicleType7() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getVehicleType8
  ******************************************************************************/
  const char_t * AbstractConfig::getVehicleType8() const
  {
    const char_t *val;
    if (!getConfig(vehicleType8Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getVehicleType8() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getVehicleType9
  ******************************************************************************/
  const char_t * AbstractConfig::getVehicleType9() const
  {
    const char_t *val;
    if (!getConfig(vehicleType9Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getVehicleType9() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getVehicleType10
  ******************************************************************************/
  const char_t * AbstractConfig::getVehicleType10() const
  {
    const char_t *val;
    if (!getConfig(vehicleType10Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getVehicleType10() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getVehicleType11
  ******************************************************************************/
  const char_t * AbstractConfig::getVehicleType11() const
  {
    const char_t *val;
    if (!getConfig(vehicleType11Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getVehicleType11() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getVehicleType12
  ******************************************************************************/
  const char_t * AbstractConfig::getVehicleType12() const
  {
    const char_t *val;
    if (!getConfig(vehicleType12Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getVehicleType12() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getVehicleType13
  ******************************************************************************/
  const char_t * AbstractConfig::getVehicleType13() const
  {
    const char_t *val;
    if (!getConfig(vehicleType13Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getVehicleType13() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getVehicleType14
  ******************************************************************************/
  const char_t * AbstractConfig::getVehicleType14() const
  {
    const char_t *val;
    if (!getConfig(vehicleType14Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getVehicleType14() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getVehicleType15
  ******************************************************************************/
  const char_t * AbstractConfig::getVehicleType15() const
  {
    const char_t *val;
    if (!getConfig(vehicleType15Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getVehicleType15() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getVehicleType16
  ******************************************************************************/
  const char_t * AbstractConfig::getVehicleType16() const
  {
    const char_t *val;
    if (!getConfig(vehicleType16Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getVehicleType16() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getVehicleType17
  ******************************************************************************/
  const char_t * AbstractConfig::getVehicleType17() const
  {
    const char_t *val;
    if (!getConfig(vehicleType17Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getVehicleType17() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getVehicleType18
  ******************************************************************************/
  const char_t * AbstractConfig::getVehicleType18() const
  {
    const char_t *val;
    if (!getConfig(vehicleType18Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getVehicleType18() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getVehicleType19
  ******************************************************************************/
  const char_t * AbstractConfig::getVehicleType19() const
  {
    const char_t *val;
    if (!getConfig(vehicleType19Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getVehicleType19() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getVehicleType20
  ******************************************************************************/
  const char_t * AbstractConfig::getVehicleType20() const
  {
    const char_t *val;
    if (!getConfig(vehicleType20Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getVehicleType20() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getMaxNrSupvGradTarget
  ******************************************************************************/
  uint8_t AbstractConfig::getMaxNrSupvGradTarget() const
  {
    uint8_t val = 0U;

    if (!getConfig(maxSupvGradTargId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getMaxNrSupvGradTarget() failed, returning invalid value");
    }

    return val;
  }

    /******************************************************************************
    * getMinSupvGradTargetDist
    ******************************************************************************/
    uint32_t AbstractConfig::getMinSupvGradTargetDist() const
  {
    uint32_t val;

    if (!getConfig(minSupvGradTargDstId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getMinSupvGradTargetDist() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getDefaultBrakeability
  ******************************************************************************/
  uint16_t AbstractConfig::getDefaultBrakeability() const
  {
    uint16_t val;

    if (!getConfig(defaultBrakeabilityId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getDefaultBrakeability() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getDefaultBrakeDelay
  ******************************************************************************/
  uint16_t AbstractConfig::getDefaultBrakeDelay() const
  {
    uint16_t val;

    if (!getConfig(defaultBrakeDelayId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getDefaultBrakeDelay() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getExpectedTimeSyncServerVersion1
  ******************************************************************************/
  uint8_t AbstractConfig::getExpectedTimeSyncServerVersion1() const
  {
    uint8_t val;

    if (!getConfig(timeSyncServerVersion1Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getExpectedTimeSyncServerVersion1() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getExpectedTimeSyncServerVersion2
  ******************************************************************************/
  uint8_t AbstractConfig::getExpectedTimeSyncServerVersion2() const
  {
    uint8_t val;

    if (!getConfig(timeSyncServerVersion2Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getExpectedTimeSyncServerVersion2() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getExpectedTimeSyncServerVersion3
  ******************************************************************************/
  uint8_t AbstractConfig::getExpectedTimeSyncServerVersion3() const
  {
    uint8_t val;

    if (!getConfig(timeSyncServerVersion3Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getExpectedTimeSyncServerVersion3() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getExpectedTimeSyncServerVersion4
  ******************************************************************************/
  uint8_t AbstractConfig::getExpectedTimeSyncServerVersion4() const
  {
    uint8_t val;

    if (!getConfig(timeSyncServerVersion4Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getExpectedTimeSyncServerVersion4() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getTractionCutoffDelay
  ******************************************************************************/
  uint8_t AbstractConfig::getTractionCutoffDelay() const
  {
    uint8_t val;

    if (!getConfig(tractionCutOffDelayId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getTractionCutoffDelay() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getRadioTimeOutYardMode
  ******************************************************************************/
  uint8_t AbstractConfig::getRadioTimeOutYardMode() const
  {
    uint8_t val;

    if (!getConfig(radioTimeOutYardModeId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getRadioTimeOutYardMode() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getEBMarginAdded
  ******************************************************************************/
  uint8_t AbstractConfig::getEBMarginAdded() const
  {
    uint8_t val;

    if (!getConfig(ebMarginAddedId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getEBMarginAdded() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getTimsManualConfTime
  ******************************************************************************/
  uint8_t AbstractConfig::getTimsManualConfTime() const
  {
    uint8_t val;

    if (!getConfig(timsManualConfTimeId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getTimsManualConfTime() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getTimsSensorTimeDiff
  ******************************************************************************/
  uint8_t AbstractConfig::getTimsSensorTimeDiff() const
  {
    uint8_t val;

    if (!getConfig(timsSensorTimeDiffId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getTimsSensorTimeDiff() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getEbPrPropagationSpeed
  ******************************************************************************/
  uint16_t AbstractConfig::getEbPrPropagationSpeed() const
  {
    uint16_t val;

    if (!getConfig(ebPrPropagationSpeedId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getEbPrPropagationSpeed() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getReleaseSpeed
  ******************************************************************************/
  uint16_t AbstractConfig::getReleaseSpeed() const
  {
    uint16_t val;

    if (!getConfig(releaseSpeedId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getReleaseSpeed() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getDepartureWarningId
  ******************************************************************************/
  uint8_t AbstractConfig::getDepartureWarningId() const
  {
    uint8_t val;

    if (!getConfig(departureWarningId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getDepartureWarningId() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getDeleteTrackMargin
  ******************************************************************************/
  uint32_t AbstractConfig::getDeleteTrackMargin() const
  {
    // Fetching percentage emergency brake margin
    const uint32_t percentageEBMargin = getEBMarginAdded();

    //Fetching configured rollaway margin
    const uint32_t rollawayMargin = getRollAwayMargin();

    // Calculate rollaway margin including percenatge eb margin
    uint32_t rollawayEBDistance = ((rollawayMargin * percentageEBMargin) / 100U) + rollawayMargin;

    //Fetching the reversing supervision margin
    const uint32_t reverseSupMargin = getRevSupMargin();

    // Caculate the reversing suepervision margin including percentatge emergency brake margin
    uint32_t reverseEBDistance = static_cast<uint32_t>(((reverseSupMargin * percentageEBMargin) / 100U) + reverseSupMargin);

    return ATC::ATCMath::maximum(reverseEBDistance, rollawayEBDistance);
  }

  /******************************************************************************
  * getVNomMargin
  ******************************************************************************/
  uint8_t AbstractConfig::getVNomMargin() const
  {
    uint8_t val;

    if (!getConfig(vNomMarginId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getVNomMargin() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getExpectedOpcMajorVersion
  ******************************************************************************/
  uint8_t AbstractConfig::getExpectedOpcMajorVersion() const
  {
    uint8_t val;

    if (!getConfig(opcMajorVersionId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getExpectedOpcMajorVersion() failed, returning invalid value");
    }

    return val;
  }
  
  /******************************************************************************
  * getExpectedOpcMiddleVersion
  ******************************************************************************/
  uint8_t AbstractConfig::getExpectedOpcMiddleVersion() const
  {
    uint8_t val;

    if (!getConfig(opcMiddleVersionId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getExpectedOpcMiddleVersion() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getExpectedOpcMinorVersion
  ******************************************************************************/
  uint8_t AbstractConfig::getExpectedOpcMinorVersion() const
  {
    uint8_t val;

    if (!getConfig(opcMinorVersionId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getExpectedOpcMinorVersion() failed, returning invalid value");
    }

    return val;
  }


  /******************************************************************************
  * initCrossCompare
  ******************************************************************************/
  void AbstractConfig::initCrossCompare() const
  {
    //lint --e{586} 'new' is acceptable during initialization

    // Add the base class (ATC) data
    Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&coreInitDone));
    Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::AbstractConfigBase>(this));

    Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::ConfigFile>(configFileCommon));
    Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::ConfigFile>(configFileRuntime));
    Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::ConfigFile>(configFileMaint));
    Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::ConfigFile>(configFileInstance));
    Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::ConfigFile>(configFileType));


    // Iterate over all the config parameters
    for (ATC::ConfigIdType id = ATC::firstConfigItem; id < ATC::maxConfigItems; ++id)
    {
      const ATC::BaseConfigItem* const item = getConfigItem(id);

      if (item != static_cast<ATC::BaseConfigItem*>(NULL))
      {
        Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex <ATC::BaseConfigItem>(item));
      }
    }
  }

}
