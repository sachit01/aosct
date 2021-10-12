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
* 2016-11-22    saprasad    Created
* 2017-01-03    spandita    Updated with TCC1 &TCC2 ip address
* 2017-02-14    saprasad    Remove the TCC1/TCC2 IP address ,add TCC(1-4) config parameter
*                           and TCC(1-4) function defination
* 2017-03-29    marlundg    Added LIG IP    
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "config.hpp"
#include "console.hpp"
#include "atc_util.hpp"
/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace Dispatcher
{
  /******************************************************************************
  * Constructor
  ******************************************************************************/
  Config::Config() : ATC::AbstractConfigBase()
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
    AbstractConfigBase::preInit();

    configFileDisp = new ATC::ConfigFile(1000U, true, "dispatcher_data.bin", "dispatcher_1.0", 500U, dispVersionMajor, dispVersionMinor);

    if (configFileDisp == NULL)
    {
      reportConfigError(__FILE__, __LINE__, "Cannot open config file");
    }
  }

  /******************************************************************************
  * init
  ******************************************************************************/
  bool Config::init(void)
  {
    // Config init will be run multiple times until all config files are read, but this part should only be run once.
    static bool runOnce = false;
    if (!runOnce)
    {
      // Dispatcher config parameter 
      //                  ID               Name            Description               Unit    Min     Max     Default         MemoryArea
      addIPaddrConfigItem(ATC::njruIpId,   "NjruIp",       "NJRU IP address",        "IP",                   "192.168.2.30", configFileDisp);
      addUint16ConfigItem(ATC::njruPortId, "NjruPort",     "NJRU Port",              "port", 30000U, 65535U, 30131U,         configFileDisp);
      addUint16ConfigItem(ATC::bdsPortId,  "BdsPort",      "BDS Port",               "port",     1U, 65535U,  5514U,         configFileDisp);
      addUint16ConfigItem(ATC::consolePortId,"ConsolePort","Console Port",           "port", 30000U, 65535U, 30165U,         configFileDisp);
      addUint8ConfigItem(ATC::bdsLevelId,  "BdsLevel",     "BDS log-level",          "level",    0U,     9U,     0U,         configFileDisp);
      addUint8ConfigItem(ATC::njruLevelId, "NjruLevel",    "NJRU log-level",         "level",    0U,     9U,     1U,         configFileDisp);
      addUint16ConfigItem(tcc1PortId,      "Tcc1Port",     "TCC1 Port (Central)",    "port", 30000U, 65535U, 30132U,         configFileDisp);
      addUint16ConfigItem(tcc2PortId,      "Tcc2Port",     "TCC2 Port (Region 1)",   "port", 30000U, 65535U, 30133U,         configFileDisp);
      addUint16ConfigItem(tcc3PortId,      "Tcc3Port",     "TCC3 Port (Region 2)",   "port", 30000U, 65535U, 30134U,         configFileDisp);
      addIPaddrConfigItem(dmi1IpId,        "Dmi1Ip",       "DMI1 IP Address",        "IP",                   "192.168.2.30", configFileDisp);
      addIPaddrConfigItem(dmi2IpId,        "Dmi2Ip",       "DMI2 IP Address",        "IP",                   "192.168.2.31", configFileDisp);
      addUint16ConfigItem(dmiPortId,       "DmiPort",      "DMI Port number",        "port", 30000U, 65535U, 30130U,         configFileDisp);
      addUint16ConfigItem(ligPortId,       "LigPort",      "LIG Port number",        "port", 30000U, 65535U, 30150U,         configFileDisp);
      addIPaddrConfigItem(ligIpId,         "LigIp",        "LIG IP Address",         "IP",                   "192.168.2.30", configFileDisp);
      addIPaddrConfigItem(remoteOpcIpId,   "RemoteOpcIp",  "Remote OPC IP Address",  "IP",                   "192.168.2.14", configFileDisp);
      addUint16ConfigItem(remoteOpcPortId, "RemoteOpcPort","Remote OPC Port",        "port", 30000U, 65535U, 50011U,         configFileDisp);
      addUint16ConfigItem(localOpcPortId,  "LocalOpcPort", "Local OPC Port",         "port", 30000U, 65535U, 50010U,         configFileDisp);
      addUint16ConfigItem(obrdPortId,      "ObrdPort",     "OBRD Port",              "port", 30000U, 65535U, 30151U,         configFileDisp);

      runOnce = true;
    }
    // Then call the base class init()
    return ATC::AbstractConfigBase::init();
  }

  /******************************************************************************
  * getDMIPort
  ******************************************************************************/
  uint16_t Config::getDMIPort() const
  {
    uint16_t val;

    if (!getConfig(dmiPortId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getDMIPort() failed, returning invalid value");
    }

    return val;
  }
  
  /******************************************************************************
  * getTCC1Port
  ******************************************************************************/
  uint16_t Config::getTCC1Port() const
  {
    uint16_t val;

    if (!getConfig(tcc1PortId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getTCC1Port() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getTCC2Port
  ******************************************************************************/
  uint16_t Config::getTCC2Port() const
  {
    uint16_t val;

    if (!getConfig(tcc2PortId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getTCC2Port() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getTCC3Port
  ******************************************************************************/
  uint16_t Config::getTCC3Port() const
  {
    uint16_t val;

    if (!getConfig(tcc3PortId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getTCC3Port() failed, returning invalid value");
    }

    return val;
  }  
  /******************************************************************************
  * getDMI1Ip
  ******************************************************************************/
  const char_t* Config::getDMI1Ip() const
  {
    const char_t* val;
    if (!getConfig(dmi1IpId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getDMI1Ip() failed, returning invalid value");
    }

    return val;
  }
  /******************************************************************************
  * getDMI2Ip
  ******************************************************************************/
  const char_t* Config::getDMI2Ip() const
  {
    const char_t* val;
    if (!getConfig(dmi2IpId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getDMI2Ip() failed, returning invalid value");
    }
    return val;
  }
  /******************************************************************************
  * getLIGIp
  ******************************************************************************/
  const char_t* Config::getLIGIp() const
  {
      const char_t* val;
      if (!getConfig(ligIpId, val))
      {
          reportConfigError(__FILE__, __LINE__, "getLIGIp() failed, returning invalid value");
      }

      return val;
  }
  /******************************************************************************
  * getLIGPort
  ******************************************************************************/
  uint16_t Config::getLIGPort() const
  {
    uint16_t val;

    if (!getConfig(ligPortId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getLIGPort() failed, returning invalid value");
    }

    return val;
  }
  
  /******************************************************************************
  * getRemoteOPCIp
  ******************************************************************************/
  const char_t* Config::getRemoteOPCIp() const
  {
    const char_t* val;
    if (!getConfig(remoteOpcIpId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getRemoteOPCIp() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getRemoteOPCPort
  ******************************************************************************/
  uint16_t Config::getRemoteOPCPort() const
  {
    uint16_t val;

    if (!getConfig(remoteOpcPortId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getRemoteOPCPort() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getOPCPort
  ******************************************************************************/
  uint16_t Config::getLocalOPCPort() const
  {
    uint16_t val;

    if (!getConfig(localOpcPortId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getLocalOPCPort() failed, returning invalid value");
    }

    return val;
  }

  /******************************************************************************
  * getOBRDPort
  ******************************************************************************/
  uint16_t Config::getOBRDPort() const
  {
    uint16_t val;

    if (!getConfig(obrdPortId, val))
    {
      reportConfigError(__FILE__, __LINE__, "getOBRDPort() failed, returning invalid value");
    }

    return val;
  }

}
