/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of AbstractConfigBase.
*
******************************************************************************/

/**********************************************************************************
*
* REVISION HISTORY :
*
* Date          Name         Changes
* ---------------------------------------------------------------------------------
* 2016-06-10    jeneman      Created
* 2016-07-07    jeneman      Removed things related to writing to NVS
* 2016-09-16    nsyed       Added individual access functions for config parameters
***********************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <cstdio>
#include "abstract_config_base.hpp"
#include "abstract_console.hpp"
#include "atc_util.hpp"
#ifndef _DISPATCHER
#include <vfw_checkpoints.h>
#endif

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

/******************************************************************************
* LOCAL DEFINITIONS
******************************************************************************/

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATC
{
  /******************************************************************************
  * Definition of static members
  ******************************************************************************/
  ConfigFile* AbstractConfigBase::configFileCommon = static_cast<ConfigFile*>(NULL);
  ConfigFile* AbstractConfigBase::configFileRuntime = static_cast<ConfigFile*>(NULL);
  ConfigFile* AbstractConfigBase::configFileMaint = static_cast<ConfigFile*>(NULL);
  ConfigFile* AbstractConfigBase::configFileInstance = static_cast<ConfigFile*>(NULL);
  ConfigFile* AbstractConfigBase::configFileType = static_cast<ConfigFile*>(NULL);
  ConfigFile* AbstractConfigBase::configFileDisp = static_cast<ConfigFile*>(NULL);

  /******************************************************************************
  * Constructor
  ******************************************************************************/
  AbstractConfigBase::AbstractConfigBase() : ProcComponent(atcConfigId, "Config", "CFG")
  {
    if (coreAbstractConfigBasePtr != 0)
    {
      // Error handler
      aosHalt(__FILE__, __LINE__, "Config Constructor already instantiated");
    }
    // Should always start uninitialized
    state = ConfigStateUninitialized;

    // And we have no config items, so all pointers in configItems should be NULL;
    memset(&configItems[0], 0, sizeof(configItems));

    // Setup single instance pointer for core access
    coreAbstractConfigBasePtr = this;

    numberOfConfigFilesToRead = 0U;
    numberOfConfigItems = 0U;
    expectedNumberOfConfigItemsToReadFromFile = 0U;
    numConfigItemsRead = 0U;
    isAnyRunTimeConfigUpdated = false;
  }

  /******************************************************************************
  * init
  ******************************************************************************/
  bool AbstractConfigBase::init()
  {
    static bool initDone = false;
    if (state == ConfigStateUninitialized)
    {
      bool configFilesOk = true;

      // Check that adaptation has done its part and set up the memory areas
      if (AbstractApplicationBase::corePtr()->getBlockNr() == ATPBlock)
      {
        if ( (configFileCommon == NULL)
          || (configFileRuntime == NULL)
          || (configFileMaint == NULL)
          || (configFileInstance == NULL)
          || (configFileType == NULL) )
        {
          reportConfigError(__FILE__, __LINE__, "ERROR: ATP memory area not found");
          configFilesOk = false;
        }
      }

      if (AbstractApplicationBase::corePtr()->getBlockNr() == DispatcherBlock)
      {
        if (configFileDisp == NULL)
        {
          reportConfigError(__FILE__, __LINE__, "ERROR: Dispatcher memory area not found");
          configFilesOk = false;
        }
      }

      if (configFilesOk)
      {
        // then set state to ConfigStateReadingFiles
        state = ConfigStateReadingFiles;

        // and actually start reading...
        debugInfo("Starting to read configuration files now... \n");
        ConfigFile::setReadHandler(&AbstractConfigBase::readConfigParameters);

        if (AbstractApplicationBase::corePtr()->getBlockNr() == ATPBlock)
        {
          numberOfConfigFilesToRead++;
          configFileCommon->readParameters();
          numberOfConfigFilesToRead++;
          configFileRuntime->readParameters();
          numberOfConfigFilesToRead++;
          configFileMaint->readParameters();
          numberOfConfigFilesToRead++;
          configFileInstance->readParameters();
          numberOfConfigFilesToRead++;
          configFileType->readParameters();
        }

        if (AbstractApplicationBase::corePtr()->getBlockNr() == DispatcherBlock)
        {
          numberOfConfigFilesToRead++;
          configFileDisp->readParameters();
        }
      }
    }
    else if (state == ConfigStateReadingFiles)
    {
      if (numberOfConfigFilesToRead == 0U)
      {
        debugInfo("All configuration files are now read!\n");

#ifndef _SIL
        // Check only valid for non-SIL, when there's a config-file to check.
        // The number of parameters in config-file shall match the defined number of parameters.
        if (expectedNumberOfConfigItemsToReadFromFile != numConfigItemsRead)
        {
          char_t buffer[256];

          const int32_t res = snprintf(&buffer[0], sizeof(buffer), "ERROR: Mismatch in number of config-parameters."
            " Expected %d but got %d\n", expectedNumberOfConfigItemsToReadFromFile, numConfigItemsRead);

          if ((res > 0)  &&  (static_cast<size_t>(res) < sizeof(buffer)))
          {
            debugInfo(&buffer[0]);
            reportConfigError(__FILE__, __LINE__, &buffer[0]);
          }
        }
#endif

        //All items have their values, and Config is ready to provide them to other components
        initDone = true;
        state = ConfigStateReady;
      }
    }
    else //(state == ConfigStateReady)
    {
      initDone = true;
    }

    return (initDone);
  }

  /******************************************************************************
  * run
  ******************************************************************************/
  void AbstractConfigBase::run(void)
  {
#ifndef _DISPATCHER
    static uint32_t cp = 0U; // Must be initialized to 0
    vfwVisitCheckPoint(&cp, "CFG_run");
#endif

    if (isAnyRunTimeConfigUpdated)
    {
      saveRunTimeParameters();
      isAnyRunTimeConfigUpdated = false;
    }
  }

  /******************************************************************************
  * basePtr
  ******************************************************************************/
  AbstractConfigBase* AbstractConfigBase::basePtr(void)
  {
    return coreAbstractConfigBasePtr;
  }

  /******************************************************************************
  * getConfig
  ******************************************************************************/
  bool AbstractConfigBase::getConfig(const ConfigIdType id, uint8_t & val) const
  {
    bool success = false;
    const BaseConfigItem* baseItem = getConfigItem(id);

    if (baseItem != NULL)
    {
      const Uint8ConfigItem* item =
        ATC::dynamicCast<const BaseConfigItem*, const Uint8ConfigItem*>(baseItem, __FILE__, __LINE__);

      success = true;
      val = item->getValue();
    }

    return success;
  }

  /******************************************************************************
  * getConfig
  ******************************************************************************/
  bool AbstractConfigBase::getConfig(const ConfigIdType id, int8_t & val) const
  {
    bool success = false;
    const BaseConfigItem* baseItem = getConfigItem(id);

    if (baseItem != NULL)
    {
      const Int8ConfigItem* item =
        ATC::dynamicCast<const BaseConfigItem*, const Int8ConfigItem*>(baseItem, __FILE__, __LINE__);

      success = true;
      val = item->getValue();
    }

    return success;
  }

  /******************************************************************************
  * getConfig
  ******************************************************************************/
  bool AbstractConfigBase::getConfig(const ConfigIdType id, uint16_t & val) const
  {
    bool success = false;
    const BaseConfigItem* baseItem = getConfigItem(id);

    if (baseItem != NULL)
    {
      const Uint16ConfigItem* item =
        ATC::dynamicCast<const BaseConfigItem*, const Uint16ConfigItem*>(baseItem, __FILE__, __LINE__);

      success = true;
      val = item->getValue();
    }

    return success;
  }

  /******************************************************************************
  * getConfig
  ******************************************************************************/
  bool AbstractConfigBase::getConfig(const ConfigIdType id, int16_t & val) const
  {
    bool success = false;
    const BaseConfigItem* baseItem = getConfigItem(id);

    if (baseItem != NULL)
    {
      const Int16ConfigItem* item =
        ATC::dynamicCast<const BaseConfigItem*, const Int16ConfigItem*>(baseItem, __FILE__, __LINE__);

      success = true;
      val = item->getValue();
    }

    return success;
  }

  /******************************************************************************
  * getConfig
  ******************************************************************************/
  bool AbstractConfigBase::getConfig(const ConfigIdType id, uint32_t & val) const
  {
    bool success = false;
    const BaseConfigItem* baseItem = getConfigItem(id);

    if (baseItem != NULL)
    {
      const Uint32ConfigItem* item =
        ATC::dynamicCast<const BaseConfigItem*, const Uint32ConfigItem*>(baseItem, __FILE__, __LINE__);

      success = true;
      val = item->getValue();
    }

    return success;
  }

  /******************************************************************************
  * getConfig
  ******************************************************************************/
  bool AbstractConfigBase::getConfig(const ConfigIdType id, int32_t & val) const
  {
    bool success = false;
    const BaseConfigItem* baseItem = getConfigItem(id);

    if (baseItem != NULL)
    {
      const Int32ConfigItem* item =
        ATC::dynamicCast<const BaseConfigItem*, const Int32ConfigItem*>(baseItem, __FILE__, __LINE__);

      success = true;
      val = item->getValue();
    }

    return success;
  }

  /******************************************************************************
  * getConfig
  ******************************************************************************/
  bool AbstractConfigBase::getConfig(const ConfigIdType id, uint64_t & val) const
  {
    bool success = false;
    const BaseConfigItem* baseItem = getConfigItem(id);

    if (baseItem != NULL)
    {
      const  Uint64ConfigItem* item =
        ATC::dynamicCast<const BaseConfigItem*, const Uint64ConfigItem*>(baseItem, __FILE__, __LINE__);

      success = true;
      val = item->getValue();
    }

    return success;
  }

  /******************************************************************************
  * getConfig
  ******************************************************************************/
  bool AbstractConfigBase::getConfig(const ConfigIdType id, int64_t & val) const
  {
    bool success = false;
    const BaseConfigItem* baseItem = getConfigItem(id);

    if (baseItem != NULL)
    {
      const Int64ConfigItem* item =
        ATC::dynamicCast<const BaseConfigItem*, const Int64ConfigItem*>(baseItem, __FILE__, __LINE__);

      success = true;
      val = item->getValue();
    }

    return success;
  }

  /******************************************************************************
  * getConfig
  ******************************************************************************/
  bool AbstractConfigBase::getConfig(const ConfigIdType id, const char_t* & val) const
  {
    bool success = false;
    const BaseConfigItem* baseItem = getConfigItem(id);

    if (baseItem != NULL)
    {
      if (baseItem->getDatatype() == BaseConfigItem::ItemDatatypeIpaddress)
      {
        const IPaddressConfigItem* ipItem =
          ATC::dynamicCast<const BaseConfigItem*, const IPaddressConfigItem*>(baseItem, __FILE__, __LINE__);

        success = true;
        val = ipItem->getValue();
      }
      else if (baseItem->getDatatype() == BaseConfigItem::ItemDatatypeText)
      {
        const StringConfigItem* stringItem =
          ATC::dynamicCast<const BaseConfigItem*, const StringConfigItem*>(baseItem, __FILE__, __LINE__);

        success = true;
        val = stringItem->getValue();
      }
      else
      {
        trace.write(3U, "Invalid config type");
      }
    }

    return success;
  }

  /******************************************************************************
  * getConfig
  ******************************************************************************/
  bool AbstractConfigBase::getConfig(const ConfigIdType id, bool & val) const
  {
    bool success = false;
    const BaseConfigItem* baseItem = getConfigItem(id);

    if (baseItem != NULL)
    {
      const BoolConfigItem* item =
        ATC::dynamicCast<const BaseConfigItem*, const BoolConfigItem*>(baseItem, __FILE__, __LINE__);

      success = true;
      val = item->getValue();
    }

    return success;
  }

  /******************************************************************************
  * getNjruIp
  ******************************************************************************/
  const char_t* AbstractConfigBase::getNjruIp() const
  {
    const char_t* val;

    if (!getConfig(njruIpId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getNjruIp() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getRuIp
  ******************************************************************************/
  const char_t* AbstractConfigBase::getRuIp() const
  {
    const char_t* val;

    if (!getConfig(ruIpId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getRuIp() failed, returning invalid value");
    }

    return val;
  }


  /******************************************************************************
  * getNjruPort
  ******************************************************************************/
  uint16_t AbstractConfigBase::getNjruPort() const
  {
    uint16_t val;

    if (!getConfig(njruPortId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getNjruPort() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getRuPort
  ******************************************************************************/
  uint16_t AbstractConfigBase::getRuPort() const
  {
    uint16_t val;

    if (!getConfig(ruPortId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getRuPort() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getBdsPort
  ******************************************************************************/
  uint16_t AbstractConfigBase::getBdsPort() const
  {
    uint16_t val;

    if (!getConfig(bdsPortId, val))
    {
      AbstractConsole::corePtr()->writeWithNewline("getBdsPort() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getConsolePort
  ******************************************************************************/
  uint16_t AbstractConfigBase::getConsolePort() const
  {
    uint16_t val;

    if (!getConfig(consolePortId, val))
    {
      AbstractConsole::corePtr()->writeWithNewline("getConsolePort() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getBdsLevel
  ******************************************************************************/
  uint8_t AbstractConfigBase::getBdsLevel() const
  {
    uint8_t val;

    if (!getConfig(bdsLevelId, val))
    {
      AbstractConsole::corePtr()->writeWithNewline("getBdsLevel() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getNjruLevel
  ******************************************************************************/
  uint8_t AbstractConfigBase::getNjruLevel() const
  {
    uint8_t val;

    if (!getConfig(njruLevelId, val))
    {
      AbstractConsole::corePtr()->writeWithNewline("getNjruLevel() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getRuLogDynValuePeriod
  ******************************************************************************/
  uint8_t AbstractConfigBase::getRuLogDynValuePeriod() const
  {
    uint8_t val;

    if (!getConfig(ruLogDynValuePeriodId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getRuLogDynValuePeriod() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getRuLogValueDiff
  ******************************************************************************/
  uint16_t AbstractConfigBase::getRuLogValueDiff() const
  {
    uint16_t val;

    if (!getConfig(ruLogValueDiffId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getRuLogValueDiff() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * setRunTimeConfigValue
  ******************************************************************************/
  bool AbstractConfigBase::setRunTimeConfigValue(const ConfigIdType id, const uint32_t valueToStoreInNVS)
  {
    bool retValue = true;
    const char_t* errorMessage = static_cast<const char_t*>(NULL);
    BaseConfigItem* item = getConfigItem(id);

    if (item != NULL)
    {
      if (configFileRuntime == item->getConfigFile())
      {
        Uint32ConfigItem* itemUint32 =
          ATC::dynamicCast<BaseConfigItem*, Uint32ConfigItem*>(item, __FILE__, __LINE__);
        if (itemUint32 != NULL)
        {
          if (valueToStoreInNVS != itemUint32->getValue())
          {
           // Set the value to the corresponding config id
           if (itemUint32->setValue(valueToStoreInNVS))
           {
             isAnyRunTimeConfigUpdated = true;
           }
           else
           {
             errorMessage = "Trying to write an out of range value";
           }
          }
        }
        else
        {
          errorMessage = "Trying to write an unsupported parameter type";
        }
      }
      else
      {
        errorMessage = "Trying to change a read-only parameter";
      }
    }
    else
    {
      errorMessage = "Trying to change an unknown parameter";
    }

    if (errorMessage != NULL)
    {
      retValue = false;
      reportConfigError(__FILE__, __LINE__, errorMessage);
    }

    return retValue;
  }


  /******************************************************************************
  * saveRunTimeParameters
  ******************************************************************************/
  void AbstractConfigBase::saveRunTimeParameters() const
  {
    VFW_Buffer buffer;
    uint8_t raw_buffer[50];

    vfwInitBuffer(&buffer, &raw_buffer[0], sizeof(raw_buffer));

    // Write the configuration version
    vfwPutU8(&buffer, configFileRuntime->getExpectedMajorVersion());
    vfwPutU8(&buffer, configFileRuntime->getExpectedMinorVersion());

    // Write each run-time parameter
    for (uint16_t id = firstConfigItem; id < maxConfigItems; id++)
    {
      const BaseConfigItem* item = getConfigItem(id);

      if (item != NULL)
      {
        if (configFileRuntime == item->getConfigFile())
        {
          vfwPutU16(&buffer, id);
          const Uint32ConfigItem* itemUint32 =
            ATC::dynamicCast<const BaseConfigItem*, const Uint32ConfigItem*>(item, __FILE__, __LINE__);

          if (itemUint32 != NULL)
          {
            vfwPutU32(&buffer, itemUint32->getValue());
          }
          else
          {
            reportConfigError(__FILE__, __LINE__, "Trying to write an unsupported parameter type");
          }
        }
      }
    }

    vfwSetFullBuffer(&buffer);

    configFileRuntime->writeParameters(&buffer);
  }

  /******************************************************************************
  * putConfigString
  ******************************************************************************/
  bool AbstractConfigBase::putConfigString(const ConfigIdType id, const char_t * const str)
  {
    bool success = false;
    char_t message[80];
    memset(&message[0], 0, sizeof(message));
    BaseConfigItem* item = getConfigItem(id);

    if (item != NULL)
    {
      success = item->parseStringToValue(str);

      if (!success)
      {
        const int32_t res = snprintf(&message[0], sizeof(message), "Invalid value for configId %d: %s", id, str);

        if ((res < 0)  ||  (static_cast<size_t>(res) >= sizeof(message)))
        {
          ATC::aosHalt(__FILE__, __LINE__, "Error in putConfigString");
        }
      }
    }
    else
    {
      const int32_t res = snprintf(&message[0], sizeof(message), "Invalid configId %d for Configuration Datas", id);

      if ((res < 0) || (static_cast<size_t>(res) >= sizeof(message)))
      {
        ATC::aosHalt(__FILE__, __LINE__, "Error in putConfigString");
      }
    }

    if (message[0] != '\0')
    {
      trace.write(1U, &message[0]);
      writeToLog(BriefLog, &message[0], __FILE__, __LINE__);
    }

    return success;
  }

  /******************************************************************************
  * getConfigInfo
  ******************************************************************************/
  bool AbstractConfigBase::getConfigInfo(const ConfigIdType id, const char_t* & itemName, const char_t* & desc, ValueBuffer& val,
    const char_t* & unit, ValueBuffer& minimum, ValueBuffer& maximum) const
  {
    bool foundAnItem = false;
    const BaseConfigItem* item = getConfigItem(id);

    if (item != NULL)
    {
      itemName = item->getName();
      desc = item->getDescriptiveText();
      unit = item->getUnitString();
      item->getValueStrings(val, maximum, minimum);
      foundAnItem = true;
    }

    return foundAnItem;
  }

  /******************************************************************************
  * getConfigInfo
  ******************************************************************************/
  bool AbstractConfigBase::getConfigInfo(const ConfigIdType id, const char_t* & itemName, ValueBuffer& val, const char_t* & unit) const
  {
    bool foundAnItem = false;
    const BaseConfigItem* item = getConfigItem(id);

    if (item != NULL)
    {
      itemName = item->getName();
      unit = item->getUnitString();
      item->getValueString(val);
      foundAnItem = true;
    }

    return foundAnItem;
  }

#ifdef _SIL
  /******************************************************************************
  * getItemInfoForVfwSim
  ******************************************************************************/
  bool AbstractConfigBase::getItemInfoForVfwSim(const ConfigIdType id, const char_t* & itemName,
    BaseConfigItem::ItemDatatype & itemType, ConfigFile* & configFile)
  {
    bool foundAnItem = false;
    BaseConfigItem* item = getConfigItem(id);

    if (item != NULL)
    {
      itemName = item->getName();
      itemType = item->getDatatype();
      configFile = item->getConfigFile();
      if (configFile != NULL)
      {
       foundAnItem = true;
      }
    }

    return foundAnItem;
  }
#endif

  /******************************************************************************
  * getConfigItem
  ******************************************************************************/
  BaseConfigItem* AbstractConfigBase::getConfigItem(const ConfigIdType id)
  {
    BaseConfigItem* configItem;

    if ((id >= firstConfigItem) && (id < maxConfigItems))
    {
      configItem = configItems[id];
    }
    else
    {
      configItem = static_cast<BaseConfigItem*>(NULL);
      reportConfigError(__FILE__, __LINE__, "getConfigItem() called with id out of range");
    }

    return configItem;
  } //lint !e1762 A const function shouldn't return a non-const pointer

  /******************************************************************************
  * getConfigItem
  ******************************************************************************/
  const BaseConfigItem* AbstractConfigBase::getConfigItem(const ConfigIdType id) const
  {
    const BaseConfigItem* configItem;

    if ((id >= firstConfigItem) && (id < maxConfigItems))
    {
      configItem = configItems[id];
    }
    else
    {
      configItem = static_cast<const BaseConfigItem*>(NULL);
      reportConfigError(__FILE__, __LINE__, "getConfigItem() called with id out of range");
    }

    return configItem;
  }

  /******************************************************************************
  * readConfigParameters
  ******************************************************************************/
  void AbstractConfigBase::readConfigParameters(const char_t* const filename, VFW_NvshHandle const handle, VFW_Buffer* const buffer)
  {
    ConfigFile* configFile = static_cast<ConfigFile*>(NULL);
    bool readOnly = true;
    bool versionOk = false;

    if (AbstractApplicationBase::corePtr()->getBlockNr() == ATPBlock)
    {
      if (handle == configFileCommon->getNvshHandle())
      {
        configFile = configFileCommon;
      }
      else if (handle == configFileType->getNvshHandle())
      {
        configFile = configFileType;
      }
      else if (handle == configFileInstance->getNvshHandle())
      {
        configFile = configFileInstance;
      }
      else if (handle == configFileMaint->getNvshHandle())
      {
        configFile = configFileMaint;
      }
      else if (handle == configFileRuntime->getNvshHandle())
      {
        configFile = configFileRuntime;
        readOnly = false;
      }
      else
      {
        // Do nothing
      }
    }

    if (AbstractApplicationBase::corePtr()->getBlockNr() == DispatcherBlock)
    {
      if (handle == configFileDisp->getNvshHandle())
      {
        configFile = configFileDisp;
      }
    }

    if (configFile != NULL)
    {
      versionOk = configFile->readAndVerifyVersion(buffer);

      if (versionOk)
      {
        AbstractConfigBase::basePtr()->readConfigParameters(handle, buffer, readOnly);
      }
    }

    if (!versionOk)
    {
      char_t errorStr[200];

      const int32_t res = snprintf(&errorStr[0], sizeof(errorStr),
        "Could not read config file: %s ", filename);
      if ((res > 0) && (static_cast<size_t>(res) < sizeof(errorStr)))
      {
        reportConfigError(__FILE__, __LINE__, &errorStr[0]);
      }
      else
      {
        reportConfigError(__FILE__, __LINE__, "Could not read config file");
      }
    }
  }

  /******************************************************************************
  * readConfigParameters
  ******************************************************************************/
  //lint -e{818} Cannot be declared as const, due to external interface
  void AbstractConfigBase::readConfigParameters(VFW_NvshHandle const handle, VFW_Buffer* const buffer, const bool readOnly)
  {
    while (0U < vfwGetValidSize(buffer))
    {
      char_t errorStr[200];
      const uint16_t id = vfwGetU16(buffer);
      BaseConfigItem* const item = getConfigItem(id);

      if (item == static_cast<BaseConfigItem*>(NULL))
      {
        const int32_t res = snprintf(&errorStr[0], sizeof(errorStr),
          "Config parameter %d not found", id);

        if ((res > 0) && (static_cast<size_t>(res) < sizeof(errorStr)))
        {
          reportConfigError(__FILE__, __LINE__, &errorStr[0]);
        }
        else
        {
          reportConfigError(__FILE__, __LINE__, "Config parameter not found");
        }
      }
      else if (handle != item->getConfigFile()->getNvshHandle())
      {
        const int32_t res = snprintf(&errorStr[0], sizeof(errorStr),
          "Config parameter %d in wrong config file", id);

        if ((res > 0) && (static_cast<size_t>(res) < sizeof(errorStr)))
        {
          reportConfigError(__FILE__, __LINE__, &errorStr[0]);
        }
        else
        {
          reportConfigError(__FILE__, __LINE__, "Config parameter in wrong config file");
        }
      }
      else if (readOnly && item->isReadFlagConfigID())
      {
        const int32_t res = snprintf(&errorStr[0], sizeof(errorStr),
          "Config parameter %d is being duplicated", id);

        if ((res > 0) && (static_cast<size_t>(res) < sizeof(errorStr)))
        {
          reportConfigError(__FILE__, __LINE__, &errorStr[0]);
        }
        else
        {
          reportConfigError(__FILE__, __LINE__, "Config parameter is being duplicated");
        }
      }
      else
      {
        if (item->readValueFromBuffer(buffer))
        {
          item->setReadFlagConfigID();
        }
        else
        {
          const int32_t res = snprintf(&errorStr[0], sizeof(errorStr),
            "Config parameter %s is out of range", item->getName());

          if ((res > 0) && (static_cast<size_t>(res) < sizeof(errorStr)))
          {
            reportConfigError(__FILE__, __LINE__, &errorStr[0]);
          }
          else
          {
            reportConfigError(__FILE__, __LINE__, "Config parameter is out of range");
          }
        }
      }

      numConfigItemsRead++;
    }

    if (numberOfConfigFilesToRead > 0U)
    {
      numberOfConfigFilesToRead--;
    }
  }

  /******************************************************************************
  * addConfigItem
  ******************************************************************************/
  void AbstractConfigBase::addConfigItem(BaseConfigItem * const pItem)
  {
    const ConfigIdType configItemId = pItem->getConfigItemId();

    if ( (configItemId >= firstConfigItem)
      && (configItemId < maxConfigItems)
      && (numberOfConfigItems < maxConfigItems) )
    {
     configItems[configItemId] = pItem;
     configParamIds[numberOfConfigItems] = configItemId;

     // Total number of config-parameters.
     numberOfConfigItems++;

     // Keep count of how many config-parameters that shall be present in configuration file.
     // (only if config-parameter is allocated to a memory area)
     const ConfigFile* configFile = pItem->getConfigFile();

     if (configFile != static_cast<ConfigFile*>(NULL))
     {
       expectedNumberOfConfigItemsToReadFromFile++;
     }
    }
    else
    {
      delete pItem;
      reportConfigError(__FILE__, __LINE__, "ERROR: getConfigItemId() or numberOfConfigItems is out of range\n");
    }
  }

  /******************************************************************************
  * addUint8ConfigItem
  ******************************************************************************/
  void AbstractConfigBase::addUint8ConfigItem(const ConfigIdType id, const char_t * const itemName, const char_t * const desc,
    const char_t * const unit, const uint8_t minimum, const uint8_t maximum, const uint8_t defVal,  ConfigFile * const configFile)
  {
    if ((maximum >= defVal) && (minimum <= defVal))
    {
      Uint8ConfigItem* const item = new Uint8ConfigItem(id, itemName, desc, unit, minimum, maximum, defVal, configFile);
      addConfigItem(item);
    }
    else
    {
      char_t  toWrite[buffSize];
      const int32_t res = snprintf(&toWrite[0], sizeof(toWrite), "%s: The default value provided is out of range", itemName);

      if ((res > 0) && (static_cast<size_t>(res) < sizeof(toWrite)))
      {
        reportConfigError(__FILE__, __LINE__, &toWrite[0]);
      }
      else
      {
        reportConfigError(__FILE__, __LINE__, "The default value provided is out of range");
      }
    }
  }

  /******************************************************************************
  * addInt8ConfigItem
  ******************************************************************************/
  void AbstractConfigBase::addInt8ConfigItem(const ConfigIdType id, const char_t * const itemName, const char_t * const desc,
    const char_t * const unit, const int8_t minimum, const int8_t maximum, const int8_t defVal,  ConfigFile * const configFile)
  {
    if ((maximum >= defVal) && (minimum <= defVal))
    {
      Int8ConfigItem* item = new Int8ConfigItem(id, itemName, desc, unit, minimum, maximum, defVal, configFile);
      addConfigItem(item);
    }
    else
    {
      char_t  toWrite[buffSize];
      const int32_t res = snprintf(&toWrite[0], sizeof(toWrite), "%s: The default value provided is out of range", itemName);

      if ((res > 0) && (static_cast<size_t>(res) < sizeof(toWrite)))
      {
        reportConfigError(__FILE__, __LINE__, &toWrite[0]);
      }
      else
      {
        reportConfigError(__FILE__, __LINE__, "The default value provided is out of range");
      }
    }
  }

  /******************************************************************************
  * addUint16ConfigItem
  ******************************************************************************/
  void AbstractConfigBase::addUint16ConfigItem(const ConfigIdType id, const char_t * const itemName, const char_t * const desc,
    const char_t * const unit, const uint16_t minimum, const uint16_t maximum, const uint16_t defVal, ConfigFile * const configFile)
  {
    if ((maximum >= defVal) && (minimum <= defVal))
    {
      Uint16ConfigItem* item = new Uint16ConfigItem(id, itemName, desc, unit, minimum, maximum, defVal, configFile);
      addConfigItem(item);
    }
    else
    {
      char_t  toWrite[buffSize];
      const int32_t res = snprintf(&toWrite[0], sizeof(toWrite), "%s: The default value provided is out of range", itemName);
      if ((res > 0) && (static_cast<size_t>(res) < sizeof(toWrite)))
      {
        reportConfigError(__FILE__, __LINE__, &toWrite[0]);
      }
      else
      {
        reportConfigError(__FILE__, __LINE__, "The default value provided is out of range");
      }
    }
  }

  /******************************************************************************
  * addInt16ConfigItem
  ******************************************************************************/
  void AbstractConfigBase::addInt16ConfigItem(const ConfigIdType id, const char_t * const itemName, const char_t * const desc,
    const char_t * const unit, const int16_t minimum, const int16_t maximum, const int16_t defVal, ConfigFile * const configFile)
  {
    if ((maximum >= defVal) && (minimum <= defVal))
    {
      Int16ConfigItem* item = new Int16ConfigItem(id, itemName, desc, unit, minimum, maximum, defVal, configFile);
      addConfigItem(item);
    }
    else
    {
      char_t  toWrite[buffSize];
      const int32_t res = snprintf(&toWrite[0], sizeof(toWrite), "%s: The default value provided is out of range", itemName);
      if ((res > 0) && (static_cast<size_t>(res) < sizeof(toWrite)))
      {
        reportConfigError(__FILE__, __LINE__, &toWrite[0]);
      }
      else
      {
        reportConfigError(__FILE__, __LINE__, "The default value provided is out of range");
      }
    }
  }

  /******************************************************************************
  * addUint32ConfigItem
  ******************************************************************************/
  void AbstractConfigBase::addUint32ConfigItem(const ConfigIdType id, const char_t * const itemName, const char_t * const desc,
    const char_t * const unit, const uint32_t minimum, const uint32_t maximum, const uint32_t defVal, ConfigFile * const configFile)
  {
    if ((maximum >= defVal) && (minimum <= defVal))
    {
      Uint32ConfigItem* item = new Uint32ConfigItem(id, itemName, desc, unit, minimum, maximum, defVal, configFile);
      addConfigItem(item);
    }
    else
    {
      char_t  toWrite[buffSize];
      const int32_t res = snprintf(&toWrite[0], sizeof(toWrite), "%s: The default value provided is out of range", itemName);
      if ((res > 0) && (static_cast<size_t>(res) < sizeof(toWrite)))
      {
        reportConfigError(__FILE__, __LINE__, &toWrite[0]);
      }
      else
      {
        reportConfigError(__FILE__, __LINE__, "The default value provided is out of range");
      }
    }
  }

  /******************************************************************************
  * addInt32ConfigItem
  ******************************************************************************/
  void AbstractConfigBase::addInt32ConfigItem(const ConfigIdType id, const char_t * const itemName, const char_t * const desc,
    const char_t * const unit, const int32_t minimum, const int32_t maximum, const int32_t defVal, ConfigFile * const configFile)
  {
    if ((maximum >= defVal) && (minimum <= defVal))
    {
      Int32ConfigItem* item = new Int32ConfigItem(id, itemName, desc, unit, minimum, maximum, defVal, configFile);
      addConfigItem(item);
    }
    else
    {
      char_t  toWrite[buffSize];
      const int32_t res = snprintf(&toWrite[0], sizeof(toWrite), "%s: The default value provided is out of range", itemName);
      if ((res > 0) && (static_cast<size_t>(res) < sizeof(toWrite)))
      {
        reportConfigError(__FILE__, __LINE__, &toWrite[0]);
      }
      else
      {
        reportConfigError(__FILE__, __LINE__, "The default value provided is out of range");
      }
    }
  }

  /******************************************************************************
  * addUint64ConfigItem
  ******************************************************************************/
  void AbstractConfigBase::addUint64ConfigItem(const ConfigIdType id, const char_t * const itemName, const char_t * const desc,
    const char_t * const unit, const uint64_t minimum, const uint64_t maximum, const uint64_t defVal, ConfigFile * const configFile)
  {
    if ((maximum >= defVal) && (minimum <= defVal))
    {
      Uint64ConfigItem* item = new Uint64ConfigItem(id, itemName, desc, unit, minimum, maximum, defVal, configFile);
      addConfigItem(item);
    }
    else
    {
      char_t  toWrite[buffSize];
      const int32_t res = snprintf(&toWrite[0], sizeof(toWrite), "%s: The default value provided is out of range", itemName);
      if ((res > 0) && (static_cast<size_t>(res) < sizeof(toWrite)))
      {
        reportConfigError(__FILE__, __LINE__, &toWrite[0]);
      }
      else
      {
        reportConfigError(__FILE__, __LINE__, "The default value provided is out of range");
      }
    }
  }

  /******************************************************************************
  * addInt64ConfigItem
  ******************************************************************************/
  void AbstractConfigBase::addInt64ConfigItem(const ConfigIdType id, const char_t * const itemName, const char_t * const desc,
    const char_t * const unit, const int64_t minimum, const int64_t maximum, const int64_t defVal, ConfigFile * const configFile)
  {
    if ((maximum >= defVal) && (minimum <= defVal))
    {
      Int64ConfigItem* item = new Int64ConfigItem(id, itemName, desc, unit, minimum, maximum, defVal, configFile);
      addConfigItem(item);
    }
    else
    {
      char_t  toWrite[buffSize];
      const int32_t res = snprintf(&toWrite[0], sizeof(toWrite), "%s: The default value provided is out of range", itemName);
      if ((res > 0) && (static_cast<size_t>(res) < sizeof(toWrite)))
      {
        reportConfigError(__FILE__, __LINE__, &toWrite[0]);
      }
      else
      {
        reportConfigError(__FILE__, __LINE__, "The default value provided is out of range");
      }
    }
  }

  /******************************************************************************
  * addStringConfigItem
  ******************************************************************************/
  void AbstractConfigBase::addStringConfigItem(const ConfigIdType id, const char_t * const itemName, const char_t * const desc,
    const char_t * const unit, const uint8_t minimum, const uint8_t maximum, const char_t * const defVal, ConfigFile * const configFile)
  {
    StringConfigItem* item = new StringConfigItem(id, itemName, desc, unit, minimum, maximum, defVal, configFile);
    addConfigItem(item);
  }

  /******************************************************************************
  * addIpaddrConfigItem
  ******************************************************************************/
  void AbstractConfigBase::addIPaddrConfigItem(const ConfigIdType id, const char_t * const itemName, const char_t * const desc,
    const char_t * const unit, const char_t * const defVal, ConfigFile * const configFile)
  {
    const uint8_t minIpAddrLen = 7U;
    const uint8_t maxIpAddrLen = valBufferSize - 1U;

    IPaddressConfigItem *item = new IPaddressConfigItem(id, itemName, desc, unit, minIpAddrLen, maxIpAddrLen, defVal, configFile);
    addConfigItem(item);
  }

  /******************************************************************************
  * addBoolConfigItem
  ******************************************************************************/
  void AbstractConfigBase::addBoolConfigItem(const ConfigIdType id, const char_t * const itemName, const char_t * const desc,
    const char_t * const unit, const bool defVal, ConfigFile * const configFile)
  {
    if ((true == defVal) || (false == defVal))
    {
      BoolConfigItem* item = new BoolConfigItem(id, itemName, desc, unit, defVal, configFile);
      addConfigItem(item);
    }
    else
    {
      char_t  toWrite[buffSize];
      const int32_t res = snprintf(&toWrite[0], sizeof(toWrite), "%s: The default value is invalid", itemName);

      if ((res > 0) && (static_cast<size_t>(res) < sizeof(toWrite)))
      {
        reportConfigError(__FILE__, __LINE__, &toWrite[0]);
      }
      else
      {
        reportConfigError(__FILE__, __LINE__, "The default value provided is out of range");
      }
    }
  }

  /******************************************************************************
  * consoleCall
  ******************************************************************************/
  bool AbstractConfigBase::consoleCall(const uint32_t argc, const ConsoleArguments argv)
  {
    /*
    This functions parses the arguments searches for the "help", "config all" or any other config
    component specific command calls and handles it. Returns true if completely handled
    else returns false. returning false will let other components handle the call. help always returns false.
    */

    bool retVal = false;

    // Handle help call at first. argc cannot be 0 as there is a check before consoleCall()
    if (isTextMatch(&argv[0][0], "help", sizeof("help")) && (argc == 1U))
    {
      const char_t* const toWrite =
        "config all    List all configuration parameters\n"
        "config <n>    Display details of config parameter with ID n";

      AbstractConsole::corePtr()->writeWithNewline(toWrite);
      retVal = false;
    }

    else if (isTextMatch(&argv[0][0], "config", sizeof("config")) && (argc == 2U))
    {
      if (isTextMatch(&argv[1][0], "all", sizeof("all")))
      {
        displayAllConfigItems();
      }
      else
      {
        ConfigIdType id;

        if (sscanf(&argv[1][0], "%hu", &id) <= 0)
        {
          //Argument is not all digits. Illegal argument
          char_t toWrite[] = "Illegal argument found";
          AbstractConsole::corePtr()->writeWithNewline(&toWrite[0]);
        }
        else
        {
          displayConfigItem(id);
        }
      }
      retVal = true;
    }
    else
    {
      retVal = false;
    }

    return retVal;
  }

  /******************************************************************************
  * displayAllConfigItems
  ******************************************************************************/
  void AbstractConfigBase::displayAllConfigItems() const
  {
    const char_t* itemName;
    ValueBuffer   val;
    const char_t* unit;

    char_t toWrite[buffSize];

    int32_t ret = snprintf(&toWrite[0], sizeof(toWrite), "%-7s %-34s %-20s %-s ", "ID", "Parameter", "Value", "Unit");

    if ((ret > 0) && (ret < buffSize))
    {
      AbstractConsole::corePtr()->writeWithNewline(&toWrite[0]);
      AbstractConsole::corePtr()->writeWithNewline(" ");
    }

    for (uint16_t i = 0U; i < numberOfConfigItems; i++)
    {
      const ConfigIdType  id = configParamIds[i];
      if (getConfigInfo(id, itemName, val, unit))
      {
        ret = snprintf(&toWrite[0], sizeof(toWrite), "%-7u %-34s %-20s %-s ", id, itemName, val.buf, unit);
        if ((ret > 0) && (ret < buffSize))
        {
          AbstractConsole::corePtr()->writeWithNewline(&toWrite[0]);
        }
      }
    }
  }

  /******************************************************************************
  * displayConfigItem
  ******************************************************************************/
  void AbstractConfigBase::displayConfigItem(const ConfigIdType id) const
  {
    const char_t* itemName;
    const char_t* desc;
    ValueBuffer   val;
    const char_t* unit;
    ValueBuffer   min;
    ValueBuffer   max;
    int32_t       ret;

    if (getConfigInfo(id, itemName, desc, val, unit, min, max))
    {
      char_t toWrite[buffSize];
      ret = snprintf(&toWrite[0], sizeof(toWrite),
        "ID:          %u\n"
        "Parameter:   %s\n"
        "Description: %s\n"
        "Value:       %s\n"
        "Unit:        %s\n"
        "Min:         %s\n"
        "Max:         %s\n",
        id, itemName, desc, val.buf, unit, min.buf, max.buf);
      if ((ret > 0) && (ret < buffSize))
      {
        AbstractConsole::corePtr()->writeWithNewline(&toWrite[0]);
      }
    }
    else
    {
      char_t toWrite[] = "No config parameter found with the given ID \n"
        "Type \"config all\" to find all the valid config parameter IDs";

      AbstractConsole::corePtr()->writeWithNewline(&toWrite[0]);
    }
  }

#ifndef _DISPATCHER

  /******************************************************************************
  * getWriteCrossCompareMaxSize
  ******************************************************************************/
  uint32_t AbstractConfigBase::getWriteCrossCompareMaxSize() const
  {
    // See writeCrossCompare below for the calculation of sizes
    return 4U + 2U + 2U + 2U + 2U + ((maxConfigItems - firstConfigItem) * 2U) + 1U;
  }

  /******************************************************************************
  * writeCrossCompare
  ******************************************************************************/
  void AbstractConfigBase::writeCrossCompare(VFW_Buffer* const buffer) const
  {
    vfwPutU32(buffer, static_cast<uint32_t>(state));
    vfwPutU16(buffer, numberOfConfigFilesToRead);
    vfwPutU16(buffer, numberOfConfigItems);
    vfwPutU16(buffer, expectedNumberOfConfigItemsToReadFromFile);
    vfwPutU16(buffer, numConfigItemsRead);

    for (uint16_t i = firstConfigItem; i < maxConfigItems; ++i)
    {
      vfwPutU16(buffer, configParamIds[i]);
    }

    vfwPutU8(buffer, isAnyRunTimeConfigUpdated ? 255U : 0U);
  }

  /******************************************************************************
  * writeCrossCompareVersions
  ******************************************************************************/
  void AbstractConfigBase::writeCrossCompareVersions(VFW_Buffer* const buffer) const
  {
    configFileCommon->writeCrossCompareVersion(buffer);
    configFileRuntime->writeCrossCompareVersion(buffer);
    configFileMaint->writeCrossCompareVersion(buffer);
    configFileInstance->writeCrossCompareVersion(buffer);
    configFileType->writeCrossCompareVersion(buffer);
  }

  /******************************************************************************
  * crossCompareVersions
  ******************************************************************************/
  bool AbstractConfigBase::crossCompareVersions(VFW_Buffer* const buffer) const
  {
    bool match;

    match = configFileCommon->crossCompareVersion(buffer);
    match = configFileRuntime->crossCompareVersion(buffer) && match;
    match = configFileMaint->crossCompareVersion(buffer) && match;
    match = configFileInstance->crossCompareVersion(buffer) && match;
    match = configFileType->crossCompareVersion(buffer) && match;

    return match;
  }

#endif

  /******************************************************************************
  * setGlobalNameToConfigParameter
  ******************************************************************************/
  void AbstractConfigBase::setGlobalNameToConfigParameter(const ConfigIdType id, const char_t* const globalName)
  {
    BaseConfigItem* item = getConfigItem(id);
    char_t msgBuf[200];

    if (item != NULL)
    {
      //Set the global name to the corresponding configuration item
      if (!(item->setGlobalName(globalName)))
      {
        const int32_t res = snprintf(&msgBuf[0], sizeof(msgBuf), "Global Name for parameter id=%d not set!", static_cast<uint32_t>(id));

        if ((res > 0) && (static_cast<size_t>(res) < sizeof(msgBuf)))
        {
          reportConfigError(__FILE__, __LINE__, &msgBuf[0]);
        }
        else
        {
          reportConfigError(__FILE__, __LINE__, "Could not setGlobalNameToConfigParameter");
        }
      }
    }
    else
    {
      const int32_t res = snprintf(&msgBuf[0], sizeof(msgBuf), "No Configuration Parameter with id=%d defined!", static_cast<uint32_t>(id));

      if ((res > 0) && (static_cast<size_t>(res) < sizeof(msgBuf)))
      {
        reportConfigError(__FILE__, __LINE__, &msgBuf[0]);
      }
      else
      {
        reportConfigError(__FILE__, __LINE__, "No Configuration Parameter defined with provided id!");
      }
    }
  }

  /******************************************************************************
  * reportConfigError
  ******************************************************************************/
  void AbstractConfigBase::reportConfigError(const char_t* const filepath, int32_t const line, const char_t * const str)
  {
    aosHalt(filepath, line, str);
  }
}
