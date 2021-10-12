/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of the Config adaptation class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-06-10    jeneman     Created
* 2017-01-12    saprasad    Implemented Analyzer port number & sendcyle for AnalyzerIF
* 2017-01-25    saprasad    Added binary files to read it by config component
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_string.h>
#include "config.hpp"
#include "atc_util.hpp"
#include "atc_math.hpp"
#include "abstract_cross_compare.hpp"

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
  BHPTypicalConfig::BHPTypicalConfig()
  {
    memset(&configName[0], 0, sizeof(configName));
    memset(&vehTypeName[0], 0, sizeof(vehTypeName));
    noOfCars = 0U;
  }

  /******************************************************************************
  * Comparison operator
  ******************************************************************************/
  bool BHPTypicalConfig::operator !=(const BHPTypicalConfig& that) const
  {
    bool diff = false;

    if (strncmp(&configName[0], &that.configName[0], sizeof(configName)) != 0)
    {
      diff = true;
    }
    if (strncmp(&vehTypeName[0], &that.vehTypeName[0], sizeof(vehTypeName)) != 0)
    {
      diff = true;
    }
    if (noOfCars != that.noOfCars)
    {
      diff = true;
    }

    return diff;
  }

  /******************************************************************************
  * Constructor
  ******************************************************************************/
  Config::Config() : AbstractConfig(), initDone(false)
  {
  }

  /******************************************************************************
  * instance
  *
  * Add additional functional description here if needed.
  * (This info is not included in doxygen documentation but may be usefull)
  *
  ******************************************************************************/
  Config& Config::instance(void)
  {
    static Config theOnlyConfigInstance;

    return theOnlyConfigInstance;
  }

  /******************************************************************************
  * preInit
  ******************************************************************************/
  void Config::preInit(void)
  {
    AbstractConfig::preInit();

    configFileCommon   = new ATC::ConfigFile(1000U, true,  "cfg_data.bin",      "cfg_1.0",      500U, commonVersionMajor, commonVersionMinor);
    configFileType     = new ATC::ConfigFile(1000U, true,  "type_data.bin",     "type_1.0",     500U, typeVersionMajor,   typeVersionMinor);
    configFileInstance = new ATC::ConfigFile(1000U, true,  "instance_data.bin", "instance_1.0", 500U, instVersionMajor,   instVersionMinor);
    configFileMaint    = new ATC::ConfigFile(1000U, true,  "mnt_data.bin",      "mnt_1.0",      500U, mntVersionMajor,    mntVersionMinor);
    configFileRuntime  = new ATC::ConfigFile(1000U, false, "rt_data.bin",       "rt_1.0",       500U, rtVersionMajor,     rtVersionMinor);

    if ( (configFileCommon == NULL)
      || (configFileRuntime == NULL)
      || (configFileMaint == NULL)
      || (configFileInstance == NULL)
      || (configFileType == NULL) )
    {
      reportConfigError(__FILE__, __LINE__, "Cannot open config file");
    }
  }

  /******************************************************************************
  * init
  ******************************************************************************/
  bool Config::init(void)
  {
    bool retValue = false;
    // Config init will be run multiple times until all config files are read, but this part should only be run once.
    if (!initDone)
    {
      // Add ATP-adaptation config items here
      addUint16ConfigItem(ticInitToRunTimeoutId, "TicInitToRunTimeout",
        "Max. time for TIC to transition from Init to Run state", "s", 10U, 1800U, 600U, configFileType);
      addUint16ConfigItem(rangeMinBP1Id, "RangeMinBp1",
        "Minimum Range value of Brake Pressure 1, Corresponding to lowest analog value from the sensor", "kPa", 0U, 100U, 0U, configFileType);
      addUint16ConfigItem(rangeMaxBP1Id, "RangeMaxBp1",
        "Maximum Range value of Brake Pressure 1, Corresponding to highest analog value from the sensor", "kPa", 0U, 5000U, 1379U, configFileType);
      addUint16ConfigItem(rangeMinBP2Id, "RangeMinBp2",
        "Minimum Range value of Brake Pressure 2, Corresponding to lowest analog value from the sensor", "kPa", 0U, 100U, 0U, configFileType);
      addUint16ConfigItem(rangeMaxBP2Id, "RangeMaxBp2",
        "Maximum Range value of Brake Pressure 2, Corresponding to highest analog value from the sensor", "kPa", 0U, 5000U, 1379U, configFileType);
      addUint16ConfigItem(maxEbApplyFbId, "MaxEbApplyFb",
        "Maximum Brake Pressure feedback to qualify as Brakes Applied", "kPa", 0U, 5000U, 100U, configFileType);
      addUint16ConfigItem(minEbReleaseFbId, "MinEbReleaseFb",
        "Minimum Brake Pressure feedback to qualify as Brakes Released", "kPa", 0U, 5000U, 500U, configFileType);
      addUint16ConfigItem(maxEbFbDiffId, "MaxEbFbDiff",
        "Maximum acceptable difference between the two Brake Pressure Sensors", "kPa", 0U, 500U, 100U, configFileType);
      addUint16ConfigItem(ebFbInaccuracyId, "EbFbInaccuracy",
        "Maximum acceptable inaccuracy in the read Brake Pressure Feedback", "kPa", 0U, 100U, 50U, configFileType);
      addUint8ConfigItem(ebFbSignalStatusId, "EbFbSignalStatus",
        "Status denoting the configuration of EB Feedback sensors.", "enum", 0U, 3U, 3U, configFileType);
      addBoolConfigItem(railRoadInputAvailId, "RailRoadInputAvail",
        "Rail/Road input available", "bool", false, configFileType);
      addBoolConfigItem(ebCutOutConfiguredId, "EbCutOutConfigured",
        "EB cut-out inputs configuration (1 : EB cut out enabled, 0: EB cut out disabled)", "bool", true, configFileType);
      addUint8ConfigItem(maxEbFbDiffTimeoutId, "MaxEbFbDiffTimeout",
        "Evaluation time for comparing EB feedback input signals BP1 and BP2", "s", 0U, 20U, 5U, configFileType);
      addUint8ConfigItem(ecpbMinPercBrakeCarsId, "EcpbMinPercBrakeCars",
        "Minimum percentage of cars with functional brakes on ECPB equipped train", "%", 85U, 100U, 85U, configFileType);
      addBoolConfigItem(nonLeadingLocoInputId, "NonLeadingLocoInput",
        "Input signal for Non leading Locomotive should be used or not.", "bool", true, configFileType);
      addUint16ConfigItem(timsMinBPLocoId, "TimsMinBpLoco",
        "Lowest allowed brake pipe pressure in locomotive to keep the train intact", "kPa", 0U, 500U, 400U, configFileType);
      addUint16ConfigItem(timsMinBPLastCarId, "TimsMinBpLastCar",
        "Lowest allowed brake pipe pressure in locomotive to keep the train intact", "kPa", 0U, 500U, 400U, configFileType);
      addUint8ConfigItem(tcoOrderAndTCOFbId, "TcoOrderAndTcoFb",
        "Whether TCO order and TCO feedback shall be used or not.", "enum", 0U, 3U, 3U, configFileType);
      addUint16ConfigItem(ebFeedbackTimeoutId, "EbFeedbackTimeout",
        "Max wait time for the expected Emergency Brake Feedback", "s", 0U, 120U, 10U, configFileType);
      addUint16ConfigItem(tcoFeedbackTimeoutId, "TcoFeedbackTimeout",
        "Max wait time for the expected TCO Feedback", "s", 0U, 120U, 10U, configFileType);
      addUint8ConfigItem(useEbTcoFbBrakeTestId, "UseEbTcoFbBrakeTest",
        "Status denoting the usage of TCO- and EB-Feedback during Brake Test.", "enum", 0U, 3U, 3U, configFileType);
      addUint8ConfigItem(lcsSampleSpeedDiffId, "LcsSampleSpeedDiff",
        "Difference of Speed to take next sample for LCS Warning Curve Message", "cm/s", 0U, 255U, 28U, configFileType);
      addUint8ConfigItem(lcsMaxSampleId, "LcsMaxSample",
        "Maximum number of sample points in LCS Warning Curve Message", "nr", 1U, 110U, 100U, configFileType);
      addUint8ConfigItem(ecpbCarInBrakeCalcId, "EcpbCarInBrakeCalc",
        "Percentage of connected ECPB cars used in brake calculations", "%", 85U, 100U, 90U, configFileType);
      addUint16ConfigItem(bpDeltaRapidLossId, "BpDeltaRapidLoss",
        "Delta pressure to trigger rapid loss of pressure", "kPa", 0U, 1500U, 1000U, configFileType);
      addUint8ConfigItem(bpTimeRapidLossId, "BpTimeRapidLoss",
        "Time window to detect rapid loss of pressure", "0.1 s", 1U, 100U, 20U, configFileType);
      addUint8ConfigItem(timsEcbpTimeOutId, "TimsEcbpTimeOut",
        "Time limit to consider TIMS as broken unless reported intact", "s", 3U, 60U, 30U, configFileType);
      addUint8ConfigItem(timsEotTimeOutId, "TimsEotTimeOut",
        "Time limit to consider TIMS as broken unless reported intact", "s", 3U, 60U, 30U, configFileType);
      addUint8ConfigItem(timsLengthMarginId, "TimsLengthMargin",
        "Margin to be used determine train length for the TIMS function", "m", 0U, 100U, 50U, configFileType);
      addStringConfigItem(typicalConfigName1Id, "TypicalCfgName1",
        "Name of a typical configuration", "str", 1U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(typicalConfigCarType1Id, "TypicalCfgCarType1",
        "Name of cars included in the typical configuration", "str", 0U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addUint8ConfigItem(typicalConfigNoOfCars1Id, "TypicalCfgNoOfCars1",
        "Number of cars included in the typical configuration", "nr", 0U, 150U, 0U, static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(typicalConfigName2Id, "TypicalCfgName2",
        "Name of a typical configuration", "str", 1U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(typicalConfigCarType2Id, "TypicalCfgCarType2",
        "Name of cars included in the typical configuration", "str", 0U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addUint8ConfigItem(typicalConfigNoOfCars2Id, "TypicalCfgNoOfCars2",
        "Number of cars included in the typical configuration", "nr", 0U, 150U, 0U, static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(typicalConfigName3Id, "TypicalCfgName3",
        "Name of a typical configuration", "str", 1U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(typicalConfigCarType3Id, "TypicalCfgCarType3",
        "Name of cars included in the typical configuration", "str", 0U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addUint8ConfigItem(typicalConfigNoOfCars3Id, "TypicalCfgNoOfCars3",
        "Number of cars included in the typical configuration", "nr", 0U, 150U, 0U, static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(typicalConfigName4Id, "TypicalCfgName4",
        "Name of a typical configuration", "str", 1U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(typicalConfigCarType4Id, "TypicalCfgCarType4",
        "Name of cars included in the typical configuration", "str", 0U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addUint8ConfigItem(typicalConfigNoOfCars4Id, "TypicalCfgNoOfCars4",
        "Number of cars included in the typical configuration", "nr", 0U, 150U, 0U, static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(typicalConfigName5Id, "TypicalCfgName5",
        "Name of a typical configuration", "str", 1U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addStringConfigItem(typicalConfigCarType5Id, "TypicalCfgCarType5",
        "Name of cars included in the typical configuration", "str", 0U, 20U, "", static_cast<ATC::ConfigFile*>(NULL));
      addUint8ConfigItem(typicalConfigNoOfCars5Id, "TypicalCfgNoOfCars5",
        "Number of cars included in the typical configuration", "nr", 0U, 150U, 0U, static_cast<ATC::ConfigFile*>(NULL));
      addUint16ConfigItem(sbPrPropagationSpeedId, "SbPrPropagationSpeed",
        "SB pressure propagation speed", "cm/s", 300U, 30000U, 1000U, configFileType);
      addUint16ConfigItem(maxExtRevDistanceId, "MaxExtRevDistance",
        "Maximum extended reversing distance. 0=Disabled", "m", 0U, 100U, 0U, configFileType);
      addUint16ConfigItem(maxExtRevSpeedId, "MaxExtRevSpeed",
        "Maximum speed allowed during Extended Reversing supervision", "cm/s", 140U, 700U, 420U, configFileType);
      addUint16ConfigItem(minApproachSpeedId, "MinApproachSpeed",
        "Minimum speed to be used ", "cm/s", 140U, 1400U, 780U, configFileType);
      addUint8ConfigItem(obrdLengthMarginId, "ObrdLengthMargin",
        "Margin for sanity check of OBRD position", "m", 0U, 100U, 50U, configFileType);
      addUint16ConfigItem(obrdBrakeTestPressureId, "ObrdBrakeTestPr",
        "Brake pressure used in the pre-departure brake test", "kPa", 0U, 5000U, 500U, configFileType);
      addUint8ConfigItem(obrdReportTimeoutId,"ObrdReportTimeout",
        "Maximum Timeout between two Consecutive OBRD Reports","s", 0U, 15U, 10U, configFileType);
      addUint16ConfigItem(brakePressureStabilizeMarginId, "BpStabilizeMargin", "Brake pressure within this margin when released pressure reached during braketest",
        "kPa/s", 5U, 100U, 20U, configFileType);
      initDone = true;
    }

    // Then call the parent class init()
    if (AbstractConfig::init())
    {
      setGlobalNameToConfigParameter(ecpbMinPercBrakeCarsId, "P_ECPBBRAKESREQUIRED");
      setGlobalNameToConfigParameter(ecpbCarInBrakeCalcId, "P_ECPBBRAKESUSED");
      setGlobalNameToConfigParameter(bpDeltaRapidLossId, "Pr_BRAKEPRAPIDLOSS");
      setGlobalNameToConfigParameter(timsMinBPLastCarId, "Pr_BRAKEPLOWEOT");
      setGlobalNameToConfigParameter(timsMinBPLocoId, "Pr_BRAKEPLOWLOCO");
      setGlobalNameToConfigParameter(bpTimeRapidLossId, "T_BRAKEPRAPIDLOSS");
      setGlobalNameToConfigParameter(timsEcbpTimeOutId, "T_TIMSECPBTIMEOUT");
      setGlobalNameToConfigParameter(timsEotTimeOutId, "T_TIMSEOTTIMEOUT");
      setGlobalNameToConfigParameter(timsLengthMarginId, "D_TIMSLENGTHMARGIN");
      setGlobalNameToConfigParameter(typicalConfigName1Id, "S_TYPICALCONFIGNAME1");
      setGlobalNameToConfigParameter(typicalConfigCarType1Id, "S_TYPICALCONFIGCAR1");
      setGlobalNameToConfigParameter(typicalConfigNoOfCars1Id, "N_TYPICALCONFIGNOOF1");
      setGlobalNameToConfigParameter(typicalConfigName2Id, "S_TYPICALCONFIGNAME2");
      setGlobalNameToConfigParameter(typicalConfigCarType2Id, "S_TYPICALCONFIGCAR2");
      setGlobalNameToConfigParameter(typicalConfigNoOfCars2Id, "N_TYPICALCONFIGNOOF2");
      setGlobalNameToConfigParameter(typicalConfigName3Id, "S_TYPICALCONFIGNAME3");
      setGlobalNameToConfigParameter(typicalConfigCarType3Id, "S_TYPICALCONFIGCAR3");
      setGlobalNameToConfigParameter(typicalConfigNoOfCars3Id, "N_TYPICALCONFIGNOOF3");
      setGlobalNameToConfigParameter(typicalConfigName4Id, "S_TYPICALCONFIGNAME4");
      setGlobalNameToConfigParameter(typicalConfigCarType4Id, "S_TYPICALCONFIGCAR4");
      setGlobalNameToConfigParameter(typicalConfigNoOfCars4Id, "N_TYPICALCONFIGNOOF4");
      setGlobalNameToConfigParameter(typicalConfigName5Id, "S_TYPICALCONFIGNAME5");
      setGlobalNameToConfigParameter(typicalConfigCarType5Id, "S_TYPICALCONFIGCAR5");
      setGlobalNameToConfigParameter(typicalConfigNoOfCars5Id, "N_TYPICALCONFIGNOOF5");
      setGlobalNameToConfigParameter(maxExtRevDistanceId, "D_EXTREVERSING");
      setGlobalNameToConfigParameter(maxExtRevSpeedId, "V_EXTREVERSINGSPEED");
      setGlobalNameToConfigParameter(minApproachSpeedId, "V_MINAPPROACHSPEED");

      retValue = true;
    }

    return retValue;
  }

  /******************************************************************************
  * getTicInitToRunTimeout
  ******************************************************************************/
  uint16_t Config::getTicInitToRunTimeout() const
  {
    uint16_t val;

    if (!getConfig(ticInitToRunTimeoutId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getTicInitToRunTimeout() failed, returning invalid value");
    }

    return val;
  }
  /******************************************************************************
  * getRangeMinBP1
  ******************************************************************************/
  uint16_t Config::getRangeMinBP1() const
  {
    uint16_t val;

    if (!getConfig(rangeMinBP1Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getRangeMinBP1() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getRangeMaxBP1
  ******************************************************************************/
  uint16_t Config::getRangeMaxBP1() const
  {
    uint16_t val;

    if (!getConfig(rangeMaxBP1Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getRangeMaxBP1() failed, returning invalid value");
    }

    return val;
  }


  /******************************************************************************
  * getRangeMinBP2
  ******************************************************************************/
  uint16_t Config::getRangeMinBP2() const
  {
    uint16_t val;

    if (!getConfig(rangeMinBP2Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getRangeMinBP2() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getRangeMaxBP2
  ******************************************************************************/
  uint16_t Config::getRangeMaxBP2() const
  {
    uint16_t val;

    if (!getConfig(rangeMaxBP2Id, val))
    {
      reportConfigError(__FILE__, __LINE__, "getRangeMaxBP2() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getMaxEbFbDiff
  ******************************************************************************/
  uint16_t Config::getMaxEbFbDiff() const
  {
    uint16_t val;

    if (!getConfig(maxEbFbDiffId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getMaxEbFbDiff() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getEbFbInaccuracy
  ******************************************************************************/
  uint16_t Config::getEbFbInaccuracy() const
  {
    uint16_t val;

    if (!getConfig(ebFbInaccuracyId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getEbFbInaccuracy() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getUseEbTcoFbDuringBrakeTest
  ******************************************************************************/
  uint8_t Config::getUseEbTcoFbDuringBrakeTest() const
  {
    uint8_t val;

    if (!getConfig(useEbTcoFbBrakeTestId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getUseEbTcoFbDuringBrakeTest() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getEbFbSignalStatus
  ******************************************************************************/
  uint8_t Config::getEbFbSignalStatus() const
  {
    uint8_t val;

    if (!getConfig(ebFbSignalStatusId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getEbFbSignalStatus() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getTcoFbSignalStatus
  ******************************************************************************/
  uint8_t Config::getTcoOrderAndTCOFb() const
  {
    uint8_t val;

    if (!getConfig(tcoOrderAndTCOFbId, val))
    {
      reportConfigError(__FILE__, __LINE__, "gettcoOrderAndTCOFb() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getMaxEbApplyFeedback
  ******************************************************************************/
  uint16_t Config::getMaxEbApplyFeedback() const
  {
    uint16_t val;

    if (!getConfig(maxEbApplyFbId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getMaxEbApplyFeedback() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  *   getMinEbReleaseFeedback
  ******************************************************************************/
  uint16_t Config::getMinEbReleaseFeedback() const
  {
    uint16_t val;

    if (!getConfig(minEbReleaseFbId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getMinEbReleaseFeedback() failed, returning invalid value");
    }
    return val;
  }


  /******************************************************************************
  * getEbCutOutConfigured
  ******************************************************************************/
  bool Config::getEbCutOutConfigured() const
  {
    bool val;

    if (!getConfig(ebCutOutConfiguredId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getEbCutOutConfigured() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getMaxEbFbDiffTimeout
  ******************************************************************************/
  uint8_t Config::getMaxEbFbDiffTimeout() const
  {
    uint8_t val;

    if (!getConfig(maxEbFbDiffTimeoutId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getMaxEbFbDiffTimeout() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getECPBMinPercBrakeCars
  ******************************************************************************/
  uint8_t Config::getECPBMinPercBrakeCars() const
  {
    uint8_t val;

    if (!getConfig(ecpbMinPercBrakeCarsId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getECPBMinPercBrakeCars() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getECPBCarsInBrakeCalc
  ******************************************************************************/
  uint8_t Config::getECPBCarsInBrakeCalc() const
  {
    uint8_t val;

    if (!getConfig(ecpbCarInBrakeCalcId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getECPBCarsInBrakeCalc() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getRailRoadInputAvail
  ******************************************************************************/
  bool Config::getRailRoadInputAvail() const
  {
    bool val;

    if (!getConfig(railRoadInputAvailId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getRailRoadInputAvail() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getNonLeadingLocoInput
  ******************************************************************************/
  bool Config::getNonLeadingLocoInput() const
  {
    bool val;

    if (!getConfig(nonLeadingLocoInputId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getNonLeadingLocoInput() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getTimsMinBPLoco
  ******************************************************************************/
  uint16_t Config::getTimsMinBPLoco() const
  {
    uint16_t val;

    if (!getConfig(timsMinBPLocoId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getTimsMinBPLoco() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getTimsMinBPLastCar
  ******************************************************************************/
  uint16_t Config::getTimsMinBPLastCar() const
  {
    uint16_t val;

    if (!getConfig(timsMinBPLastCarId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getTimsMinBPLastCar() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getLCSSpeedDiffForSample
  ******************************************************************************/
  uint8_t Config::getLCSSpeedDiffForSample() const
  {
    uint8_t val = 0U;

    if (!getConfig(lcsSampleSpeedDiffId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getLCSSpeedDiffForSample() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getLCSMaxSample
  ******************************************************************************/
  uint8_t Config::getLCSMaxSample() const
  {
    uint8_t val = 0U;

    if (!getConfig(lcsMaxSampleId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getLCSMaxSample() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getEbFeedbackTimeout
  ******************************************************************************/
  uint16_t Config::getEbFeedbackTimeout() const
  {
    uint16_t val;

    if (!getConfig(ebFeedbackTimeoutId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getEbFeedbackTimeout() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getTcoFeedbackTimeout
  ******************************************************************************/
  uint16_t Config::getTcoFeedbackTimeout() const
  {
    uint16_t val;

    if (!getConfig(tcoFeedbackTimeoutId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getTcoFeedbackTimeout() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getBPDeltaRapidLoss
  ******************************************************************************/
  uint16_t Config::getBPDeltaRapidLoss() const
  {
    uint16_t val;

    if (!getConfig(bpDeltaRapidLossId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getBPDeltaRapidLoss() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getBPTimeRapidLoss
  ******************************************************************************/
  uint8_t Config::getBPTimeRapidLoss() const
  {
    uint8_t val;

    if (!getConfig(bpTimeRapidLossId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getBPTimeRapidLoss() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getTimsEcbpTimeOut
  ******************************************************************************/
  uint8_t Config::getTimsEcbpTimeOut() const
  {
    uint8_t val;

    if (!getConfig(timsEcbpTimeOutId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getTimsEcbpTimeOut() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getTimsEotTimeOut
  ******************************************************************************/
  uint8_t Config::getTimsEotTimeOut() const
  {
    uint8_t val;

    if (!getConfig(timsEotTimeOutId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getTimsEotTimeOut() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getTimsLengthMargin
  ******************************************************************************/
  uint8_t Config::getTimsLengthMargin() const
  {
    uint8_t val;

    if (!getConfig(timsLengthMarginId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getTimsLengthMargin() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getSbPrPropagationSpeed
  ******************************************************************************/
  uint16_t Config::getSbPrPropagationSpeed() const
  {
    uint16_t val;

    if (!getConfig(sbPrPropagationSpeedId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getSbPrPropagationSpeed() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getOBRDReportTimeout
  ******************************************************************************/
  uint8_t Config::getOBRDReportTimeout() const
  {
    uint8_t  val;

    if (!getConfig(obrdReportTimeoutId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getOBRDReportTimeout() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getExtReversingDistance
  ******************************************************************************/
  uint16_t Config::getMaxExtReversingDistance() const
  {
    uint16_t val;

    if (!getConfig(maxExtRevDistanceId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getMaxExtReversingDistance() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getExtReversingSpeed
  ******************************************************************************/
  uint16_t Config::getMaxExtReversingSpeed() const
  {
    uint16_t val;

    if (!getConfig(maxExtRevSpeedId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getMaxExtReversingSpeed() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getMinApproachSpeed
  ******************************************************************************/
  uint16_t Config::getMinApproachSpeed() const
  {
    uint16_t val;

    if (!getConfig(minApproachSpeedId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getMinApproachSpeed() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getOBRDLengthMargin
  ******************************************************************************/
  uint8_t Config::getOBRDLengthMargin() const
  {
    uint8_t val;

    if (!getConfig(obrdLengthMarginId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getOBRDLengthMargin() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getOBRDBrakeTestPressure
  ******************************************************************************/
  uint16_t Config::getOBRDBrakeTestPressure() const
  {
    uint16_t val;

    if (!getConfig(obrdBrakeTestPressureId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getOBRDBrakeTestPressure() failed, returning invalid value");
    }
    return val;
  }

  /******************************************************************************
  * getBrakePressureStabilizeMargin
  ******************************************************************************/
  uint16_t Config::getBrakePressureStabilizeMargin() const
  {
    uint16_t val;

    if (!getConfig(brakePressureStabilizeMarginId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getBrakePressureStabilizeMargin() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getDeleteTrackMargin
  ******************************************************************************/
  uint32_t Config::getDeleteTrackMargin() const
  {
    uint32_t retVal = 0U;
    const uint32_t extdReversingSupvDistance = getMaxExtReversingDistance();

    if (extdReversingSupvDistance == 0U)
    {
      retVal = AbstractConfig::getDeleteTrackMargin();
    }
    else
    {
      // Fetching percentage emergency brake margin
      const uint32_t percentageEBMargin = getEBMarginAdded();

      //Fetching configured rollaway margin
      const uint32_t rollawayMargin = getRollAwayMargin();

      // Calculate rollaway margin including percenatge eb margin
      const uint32_t rollawayEBMargin = ((rollawayMargin * percentageEBMargin) / 100U) + rollawayMargin;

      // Calculate extended reversing supervision distance including eb margin
      const uint32_t extReversingSupvEBDistance = ((extdReversingSupvDistance * percentageEBMargin) / 100U) + extdReversingSupvDistance;

      retVal = ATC::ATCMath::maximum(rollawayEBMargin, extReversingSupvEBDistance);
    }
    return retVal;
  }
  /******************************************************************************
  * getTypicalConfig
  ******************************************************************************/
  void Config::getTypicalConfig(const uint8_t configIndex, BHPTypicalConfig& config) const
  {
    switch (configIndex)
    {
    case 0:
      getTypicalConfig(typicalConfigName1Id, typicalConfigCarType1Id, typicalConfigNoOfCars1Id, config);
      break;
    case 1:
      getTypicalConfig(typicalConfigName2Id, typicalConfigCarType2Id, typicalConfigNoOfCars2Id, config);
      break;
    case 2:
      getTypicalConfig(typicalConfigName3Id, typicalConfigCarType3Id, typicalConfigNoOfCars3Id, config);
      break;
    case 3:
      getTypicalConfig(typicalConfigName4Id, typicalConfigCarType4Id, typicalConfigNoOfCars4Id, config);
      break;
    case 4:
      getTypicalConfig(typicalConfigName5Id, typicalConfigCarType5Id, typicalConfigNoOfCars5Id, config);
      break;
    default:
      config = BHPTypicalConfig();
      break;
    }
  }

  /******************************************************************************
  * getTypicalConfig
  ******************************************************************************/
  void Config::getTypicalConfig(
    const ATC::ConfigIdType configNameId, const ATC::ConfigIdType vehicleTypeId,
    const ATC::ConfigIdType noOfCarsId, BHPTypicalConfig& config) const
  {
    const char_t* value;
    bool error = false;

    if (getConfig(configNameId, value))
    {
      static_cast<void>(vfw_strlcpy(&config.configName[0], value, sizeof(config.configName)));
    }
    else
    {
      error = true;
    }

    if (getConfig(vehicleTypeId, value))
    {
      static_cast<void>(vfw_strlcpy(&config.vehTypeName[0], value, sizeof(config.vehTypeName)));
    }
    else
    {
      error = true;
    }

    if (!getConfig(noOfCarsId, config.noOfCars))
    {
      error = true;
    }

    if (error)
    {
      config = BHPTypicalConfig();
      reportConfigError(__FILE__, __LINE__, "getTypicalConfig() failed, returning invalid value");
    }
  }

  /******************************************************************************
  * getMinValLocoType
  ******************************************************************************/
  uint8_t Config::getMinValLocoType() const
  {
    // Locomotive type: 0 = EMD, 1 = TMM, 2 = HiRail
    return(0U);
  }

  /******************************************************************************
  * getMaxValLocoType
  ******************************************************************************/
  uint8_t Config::getMaxValLocoType() const
  {
    // Locomotive type: 0 = EMD, 1 = TMM, 2 = HiRail
    return(2U);
  }

  /******************************************************************************
  * getDefaultValLocoType
  ******************************************************************************/
  uint8_t Config::getDefaultValLocoType() const
  {
    // Locomotive type: 0 = EMD
    return(0U);
  }

  /******************************************************************************
  * initCrossCompare
  ******************************************************************************/
  void Config::initCrossCompare() const
  {
    AbstractConfig::initCrossCompare();

    //lint --e{586} 'new' is acceptable during initialization

    Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&initDone));
  }
}
