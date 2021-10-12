/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
* This file implements the methods of the ModeControl class
* which contains the adapted functionality of the ModeControl
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-03    arastogi    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "mode_control.hpp"
#include "vehicle_com.hpp"
#include "train_config_mode_bhp.hpp"
#include "location_mode_bhp.hpp"
#include "driver_login_seq_bhp.hpp"
#include "config.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include "mode_control_event_ids.hpp"

#ifndef __GNUG__
extern "C" const char* vfwVersion(void);
#else
#include <vfw_version.h>
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
  /**
  * Logs configuration version using the given event.
  *
  * @param[in] event        The event to report
  * @param[in] majorVersion The major version number to report
  * @param[in] minorVersion The minor version number to report
  */
  void logConfigVersion(const ATC::Event& event, const uint8_t majorVersion, const uint8_t minorVersion)
  {
    char_t dynamicText[ATC::Event::maxDynamicTextLen + 1U] = { '\0' };

    const int32_t res = snprintf(&dynamicText[0], sizeof(dynamicText), "%d.%d", majorVersion, minorVersion);
    if ((res > 0) && (static_cast<size_t>(res) < sizeof(dynamicText)))
    {
      event.setDynamicText(&dynamicText[0]);
    }
    else
    {
      ATC::aosHalt(__FILE__, __LINE__, "Could not log config version");
    }

    ATC::AbstractEventHandler::corePtr()->reportEvent(event, __FILE__, __LINE__);
  }
}

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    /******************************************************************************
    * Constructor
    ******************************************************************************/
    ModeControl::ModeControl(void) : AbstractModeControl(),
    bhpbConfigVersion(0U),
    swNameAndVersion(ATC::Event::createLogEvent(atpModeControlId, ATC::AdaptationContainer, eventIdSWNameAndVersion, 0x0U,
      "ATP software version: ", true)),
    commonConfigNameAndVersion(ATC::Event::createLogEvent(atpModeControlId, ATC::AdaptationContainer, eventIdCommonConfigNameAndVersion, 0x0U,
      "Common configuration version: ", true)),
    typeConfigNameAndVersion(ATC::Event::createLogEvent(atpModeControlId, ATC::AdaptationContainer, eventIdTypeConfigNameAndVersion, 0x0U,
      "Type configuration version: ", true)),
    instanceConfigNameAndVersion(ATC::Event::createLogEvent(atpModeControlId, ATC::AdaptationContainer, eventIdInstanceConfigNameAndVersion, 0x0U,
      "Instance configuration version: ", true)),
    maintConfigNameAndVersion(ATC::Event::createLogEvent(atpModeControlId, ATC::AdaptationContainer, eventIdMaintConfigNameAndVersion, 0x0U,
      "Maintenance configuration version: ", true)),
    runtimeConfigNameAndVersion(ATC::Event::createLogEvent(atpModeControlId, ATC::AdaptationContainer, eventIdRuntimeConfigNameAndVersion, 0x0U,
      "Runtime configuration version: ", true)),
    hwDigitalInputNameAndVersion(ATC::Event::createLogEvent(atpModeControlId, ATC::AdaptationContainer, eventIdHWDigitalInputNameAndVersion, 0x0U,
      "ATP digital input hardware revision: ", true)),
    hwDigitalOutputNameAndVersion(ATC::Event::createLogEvent(atpModeControlId, ATC::AdaptationContainer, eventIdHWDigitalOutputNameAndVersion, 0x0U,
      "ATP digital output hardware revision: ", true)),
    hwAnalogInputNameAndVersion(ATC::Event::createLogEvent(atpModeControlId, ATC::AdaptationContainer, eventIdHWAnalogInputNameAndVersion, 0x0U,
      "ATP analog input hardware revision: ", true)),
    vitalframeworkNameAndVersion(ATC::Event::createLogEvent(atpModeControlId, ATC::AdaptationContainer, eventIdVFWNameAndVersion, 0x0U,
      "VFW version: ", true)),
    sdpNameAndVersion(ATC::Event::createLogEvent(atpModeControlId, ATC::AdaptationContainer, eventIdSDPNameAndVersion, 0x0U,
      "SDP Version: ", true)),
    dispatcherNameAndVersion(ATC::Event::createLogEvent(atpModeControlId, ATC::AdaptationContainer, eventIdDispatcherNameAndVersion, 0x0U,
      "Dispatcher software version: ", true)),
    viohClientNameAndVersion(ATC::Event::createLogEvent(atpModeControlId, ATC::AdaptationContainer, eventIdViohClientNameAndVersion, 0x0U,
      "VIOH client version: ", true)),
    viohServerNameAndVersion(ATC::Event::createLogEvent(atpModeControlId, ATC::AdaptationContainer, eventIdViohServerNameAndVersion, 0x0U,
      "VIOH server version: ", true)),
    opcVersionEvent(ATC::Event::createLogEvent(atpModeControlId, ATC::AdaptationContainer, eventIdOpcVersion, 0x0U,
      "OPC version: ", true)),
    adsVersionMismatch(ATC::Event::createLogEvent(atpModeControlId, ATC::AdaptationContainer,
        eventIdADSVersionMismatch, 0x0U, "ADS Version mis-match, LCS ADS version:", true))
    {
       initDone = false;
    }

    /******************************************************************************
    * instance
    *
    * Add additional functional description here if needed.
    * (This info is not included in doxygen documentation but may be useful)
    *
    ******************************************************************************/
    ModeControl& ModeControl::instance(void)
    {
      static ModeControl theOnlyModeControlInstance;

      return theOnlyModeControlInstance;
    }

    /******************************************************************************
    * createTrainConfigModeObj
    ******************************************************************************/
    AbstractMode* ModeControl::createTrainConfigModeObj()
    {
      return new TrainConfigModeBHP();
    }

    /******************************************************************************
    * createLocationModeObj
    ******************************************************************************/
    AbstractMode* ModeControl::createLocationModeObj()
    {
      return new LocationModeBHP();
    }

    /******************************************************************************
    * createDriverLoginSeqObj
    ******************************************************************************/
    DriverLoginSeq* ModeControl::createDriverLoginSeqObj()
    {
      return new DriverLoginSeqBHP();
    }

    /******************************************************************************
    * handleLocationData
    ******************************************************************************/
    void ModeControl::handleLocationData()
    {
      commonData.freeRolling = false;
    }

    /******************************************************************************
    * getConfirmTrainLoadedStateNeeded
    ******************************************************************************/
    bool ModeControl::getConfirmTrainLoadedStateNeeded() const
    {
      const TrainConfigModeBHP* configModeBHP =
        ATC::dynamicCast<const AbstractMode*, const TrainConfigModeBHP*>(getModeObj(ATPModeConfiguration), __FILE__, __LINE__);

      const bool confirmNeeded = configModeBHP->getConfirmTrainLoadedStateNeeded();

      return confirmNeeded;
    }

    /******************************************************************************
    * getLastCarBrakePressureTestAborted
    ******************************************************************************/
    bool ModeControl::getLastCarBrakePressureTestAborted() const
    {
      const TrainConfigModeBHP* configModeBHP =
        ATC::dynamicCast<const AbstractMode*, const TrainConfigModeBHP*>(getModeObj(ATPModeConfiguration), __FILE__, __LINE__);

      return  configModeBHP->getLastCarBrakePressureTestAborted();
    }

    /******************************************************************************
    * isManualAbortConfirmationNeeded
    ******************************************************************************/
    bool ModeControl::isManualAbortConfirmationNeeded() const
    {
      const TrainConfigModeBHP* configModeBHP =
        ATC::dynamicCast<const AbstractMode*, const TrainConfigModeBHP*>(getModeObj(ATPModeConfiguration), __FILE__, __LINE__);

      return configModeBHP->isManualAbortConfirmationNeeded();
    }

    /******************************************************************************
    * setStaticConfigurationVersion
    ******************************************************************************/
    void ModeControl::setStaticConfigurationVersion(const uint8_t configMajorVersion, const uint8_t configMinorVersion)
    {
      uint16_t versionOfAds;
       bhpbConfigVersion = static_cast<uint16_t>(static_cast<uint16_t>(configMajorVersion) << 8U) | configMinorVersion;

       if (TG::VehicleCom::instance().getVersionOfAdsMap(versionOfAds))
       {
         if (versionOfAds != bhpbConfigVersion)
         {
           // Report to TCC if the version of the ADS map received from LCS does not match the BHPB config version from TCC.
           adsVersionMismatch.setDynamicText(static_cast<uint32_t>(versionOfAds));
           ATC::AbstractEventHandler::corePtr()->reportEvent(adsVersionMismatch, __FILE__, __LINE__);
         }
       }
    }

    /******************************************************************************
    * init
    ******************************************************************************/
    bool ModeControl::init(void)
    {
       bool coreInitValue = AbstractModeControl::init();
       if ((!initDone) && coreInitValue)
       {
          // Cross compare
          initCrossCompare();
          initDone = true;
       }

       return initDone;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void ModeControl::initCrossCompare() const
    {
       AbstractModeControl::initCrossCompare();

       //lint --e{586} 'new' is acceptable during initialization
       Support::AbstractCrossCompare* const crossCompare = Support::AbstractCrossCompare::corePtr();

       crossCompare->addCrossCompareData(new Support::CrossCompareUint16(&bhpbConfigVersion));
       crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&swNameAndVersion));
       crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&commonConfigNameAndVersion));
       crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&typeConfigNameAndVersion));
       crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&instanceConfigNameAndVersion));
       crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&maintConfigNameAndVersion));
       crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&runtimeConfigNameAndVersion));
       crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&hwDigitalInputNameAndVersion));
       crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&hwDigitalOutputNameAndVersion));
       crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&hwAnalogInputNameAndVersion));
       crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&vitalframeworkNameAndVersion));
       crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&sdpNameAndVersion));
       crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&dispatcherNameAndVersion));       
       crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&viohClientNameAndVersion));
       crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&viohServerNameAndVersion));
       crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&opcVersionEvent));
       crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&adsVersionMismatch));
       crossCompare->addCrossCompareData(new Support::CrossCompareBool(&initDone));
    }

    /******************************************************************************
    * logVersionToTCC
    ******************************************************************************/
    void ModeControl::logVersionToTCC(void)
    {
       //log events to TCC with SW name and version
       swNameAndVersion.setDynamicText(ATC::AbstractApplicationBase::corePtr()->getApplicationVersionString());
       ATC::AbstractEventHandler::corePtr()->reportEvent(swNameAndVersion, __FILE__, __LINE__);

       const char_t* const dispatcherVersion = ATP::Kernel::ATPApplication::instance().getDispatcherVersionString();
       if (dispatcherVersion[0] != '\0')
       {
         dispatcherNameAndVersion.setDynamicText(dispatcherVersion);
       }
       else
       {
         ATC::aosHalt(__FILE__, __LINE__, "Could not log dispatcher version");
       }
       ATC::AbstractEventHandler::corePtr()->reportEvent(dispatcherNameAndVersion, __FILE__, __LINE__);

       //log events to TCC with Configuration name and version
       logConfigVersion(commonConfigNameAndVersion, commonVersionMajor, commonVersionMinor);
       logConfigVersion(typeConfigNameAndVersion, typeVersionMajor, typeVersionMinor);
       logConfigVersion(instanceConfigNameAndVersion,instVersionMajor, instVersionMinor);
       logConfigVersion(maintConfigNameAndVersion, mntVersionMajor, mntVersionMinor);
       logConfigVersion(runtimeConfigNameAndVersion, rtVersionMajor, rtVersionMinor);

       //Get the IO Revision Id
       uint32_t digInputRevId;
       uint32_t digOutputRevId;
       uint32_t analogInputRevId;
       IO::AbstractLocoIO::corePtr()->getRevisionId(digInputRevId,digOutputRevId,analogInputRevId);

       //log events to TCC with Digital Input name and version
       hwDigitalInputNameAndVersion.setDynamicText(digInputRevId);

       ATC::AbstractEventHandler::corePtr()->reportEvent(hwDigitalInputNameAndVersion, __FILE__, __LINE__);

       //log events to TCC with Digital Output name and version
       hwDigitalOutputNameAndVersion.setDynamicText(digOutputRevId);
       ATC::AbstractEventHandler::corePtr()->reportEvent(hwDigitalOutputNameAndVersion, __FILE__, __LINE__);

       //log events to TCC with Analog Input name and version
       hwAnalogInputNameAndVersion.setDynamicText(analogInputRevId);
       ATC::AbstractEventHandler::corePtr()->reportEvent(hwAnalogInputNameAndVersion, __FILE__, __LINE__);

       //log events to TCC with VFW name and version
       vitalframeworkNameAndVersion.setDynamicText(vfwVersion());
       ATC::AbstractEventHandler::corePtr()->reportEvent(vitalframeworkNameAndVersion, __FILE__, __LINE__);

       //Get the IO Revision Id
       uint8_t sdpMajorVersion;
       uint8_t sdpMiddleVersion;
       uint8_t sdpMinorVersion;
       Pos::AbstractOdometry::corePtr()->getSDPVersion(sdpMajorVersion, sdpMiddleVersion, sdpMinorVersion);

       //log events to TCC with SDP name and version
       char_t tempSDP[ATC::Event::maxDynamicTextLen + 1U] = { '\0' };
       const int32_t res = snprintf(&tempSDP[0], sizeof(tempSDP), "%d.%d.%d", sdpMajorVersion, sdpMiddleVersion, sdpMinorVersion);

       if ((res > 0) && (static_cast<size_t>(res) < sizeof(tempSDP)))
       {
         sdpNameAndVersion.setDynamicText(&tempSDP[0]);
       }
       else
       {
         ATC::aosHalt(__FILE__, __LINE__, "SDP ver error");
       }
       ATC::AbstractEventHandler::corePtr()->reportEvent(sdpNameAndVersion, __FILE__, __LINE__);

       //log events to TCC with vioh client name and version
       const char_t* const viohClientVersionString = ATP::IO::AbstractLocoIO::corePtr()->getViohClientVersionString();

       if (viohClientVersionString[0] != '\0')
       {
         viohClientNameAndVersion.setDynamicText(viohClientVersionString);
       }
       else
       {
         ATC::aosHalt(__FILE__, __LINE__, "Vioh client ver error");
       }
       ATC::AbstractEventHandler::corePtr()->reportEvent(viohClientNameAndVersion, __FILE__, __LINE__);

       //log events to TCC with vioh server name and version
       const char_t* const viohServerVersionString = ATP::IO::AbstractLocoIO::corePtr()->getViohServerVersionString();

       if (viohServerVersionString[0] != '\0')
       {
         viohServerNameAndVersion.setDynamicText(viohServerVersionString);
       }
       else
       {
         ATC::aosHalt(__FILE__, __LINE__, "Vioh Server ver error");
       }
       ATC::AbstractEventHandler::corePtr()->reportEvent(viohServerNameAndVersion, __FILE__, __LINE__);

       //log events to TCC with OPC version
       const char_t* const opcVersionString = IO::AbstractBTMHandler::corePtr()->getOpcVersionString().versionString;

       if (opcVersionString[0] != '\0')
       {
         opcVersionEvent.setDynamicText(opcVersionString);
       }

       ATC::AbstractEventHandler::corePtr()->reportEvent(opcVersionEvent, __FILE__, __LINE__);
    }

    /******************************************************************************
    * isAllowedToDisplayHandlingDone
    ******************************************************************************/
    bool ModeControl::isAllowedToDisplayHandlingDone() const
    {
      bool retFlag = false;
      const bool isFreeRollingActive =  getFreeRolling();
      // Free rolling still active in ATP
      if (isFreeRollingActive)
      { // Not allowed to request Handling done until free-rolling is deactivated.
        retFlag = false;
      }
      else
      {
        // Check if free-rolling reported by LCS
        TG::FreeRollingStatusType freeRollingStatus = TG::FreeRollingStatusNormalOperation;
        if (TG::VehicleCom::instance().getFreeRollingStatus(freeRollingStatus))
        { 
          // free rolling status from LCS is valid
          if (TG::FreeRollingStatusFreeRoll == freeRollingStatus)
          {
            // free rolling still reported by LCS
            retFlag = false;
          }
          else
          {
            // let the core decide if Handling Done is allowed
            retFlag = AbstractModeControl::isAllowedToDisplayHandlingDone();
          }
        }
        else
        { 
          // let the core decide if Handling Done is allowed
          retFlag = AbstractModeControl::isAllowedToDisplayHandlingDone();
        }
      }
      return retFlag;
    }

    /******************************************************************************
    * getSbBrakeTestRequested
    ******************************************************************************/
    bool ModeControl::getSbBrakeTestRequested() const
    {
      return getTrainConfigModeState() == TrainConfigModeBHP::trainConfigCheckValidBrkPrFeedBack;
    }
  }
}

//lint +esym(586,snprintf)
