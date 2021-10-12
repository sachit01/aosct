/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2020
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  ConfigFile reads/writes a configuration file using VFW NVSH.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2020-02-17    csundin     Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <cstdio>
#include "atc_util.hpp"
#include "config_file.hpp"
#include "abstract_config_base.hpp"
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
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATC
{
  /******************************************************************************
  * Constructor
  ******************************************************************************/
  ConfigFile::ConfigFile(const uint32_t size, const bool readOnlyParam, const char_t* const filename,
    const char_t* const uniqueId, const uint32_t responseTimeout, const uint8_t majorVersion, const uint8_t minorVersion)
  :
    isReadonly(readOnlyParam),
    expectedMajorVersion(majorVersion),
    expectedMinorVersion(minorVersion),
    foundMajorVersion(0U),
    foundMinorVersion(0U)
  {
    if (isReadonly)
    {
      nvshHandle = vfwNvshAllocateConfiguration(filename, uniqueId, size,
        &nvshErrorCallback, &nvshReadCallback, responseTimeout);
    }
    else
    {
#ifdef _SIL
      nvshHandle = vfwNvshAllocateConfiguration(filename, uniqueId, size,
        &nvshErrorCallback, &nvshReadCallback, responseTimeout);
#else
      nvshHandle = vfwNvshAllocate(filename, uniqueId, size,
        &nvshErrorCallback, &nvshReadCallback, &nvshConsistencyHandler, responseTimeout, readbackTimeOut);
#endif
    }
  }

  /******************************************************************************
  * setReadHandler
  ******************************************************************************/
  void ConfigFile::setReadHandler(ConfigReadHandler* const configReadHandler)
  {
    readHandler = configReadHandler;
  }

  /******************************************************************************
  * readParameters
  ******************************************************************************/
  void ConfigFile::readParameters() const
  {
    vfwNvshReadRequest(nvshHandle);
  }

  /******************************************************************************
  * writeParameters
  ******************************************************************************/
  void ConfigFile::writeParameters(VFW_Buffer* const buffer)
  {
    if (!isReadonly)
    {
      vfwNvshWrite(nvshHandle, buffer);
    }
    else
    {
      aosHalt(__FILE__, __LINE__, "Attempt to write to a read-only config file");
    }
  }

  /******************************************************************************
  * getNvshHandle
  ******************************************************************************/
  VFW_NvshHandle ConfigFile::getNvshHandle(void)
  {
    return nvshHandle;
  }

  /******************************************************************************
  * getExpectedMajorVersion
  ******************************************************************************/
  uint8_t ConfigFile::getExpectedMajorVersion() const
  {
    return expectedMajorVersion;
  }

  /******************************************************************************
  * getExpectedMinorVersion
  ******************************************************************************/
  uint8_t ConfigFile::getExpectedMinorVersion() const
  {
    return expectedMinorVersion;
  }

  /******************************************************************************
  * readAndVerifyVersion
  ******************************************************************************/
  bool ConfigFile::readAndVerifyVersion(VFW_Buffer* const buffer)
  {
    foundMajorVersion = vfwGetU8(buffer);
    foundMinorVersion = vfwGetU8(buffer);

    return verifyVersion(foundMajorVersion, foundMinorVersion);
  }

  /******************************************************************************
  * verifyVersion
  ******************************************************************************/
  bool ConfigFile::verifyVersion(const uint8_t majorVersion, const uint8_t minorVersion) const
  {
    const bool versionOk = (majorVersion == expectedMajorVersion) && (minorVersion == expectedMinorVersion);

    if (!versionOk)
    {
      char_t errorStr[200];

      const int32_t res = snprintf(&errorStr[0], sizeof(errorStr),
        "Configuration file has wrong version: %d.%d (expected %d.%d)\n",
        majorVersion, minorVersion, expectedMajorVersion, expectedMinorVersion);
      if ((res > 0) && (static_cast<size_t>(res) < sizeof(errorStr)))
      {
        debugInfo(&errorStr[0]);
      }
      else
      {
        debugInfo("Configuration file has wrong version\n");
      }
    }

    return versionOk;
  }

  /******************************************************************************
  * nvshReadCallback
  ******************************************************************************/
  //lint -stack(ATC::ConfigFile::nvshReadCallback(400))
  void ConfigFile::nvshReadCallback(const VFW_NvshHandle handle, VFW_Buffer* const buffer)
  {
#ifndef _DISPATCHER
    static uint32_t beginCp = 0U; // Must be initialized to 0
    vfwVisitCheckPoint(&beginCp, "CFG_nvshReadCallback_begin");
#endif

    const char_t* const filename = vfwNvshGetFilename(handle);
    char_t strBuf[100];

    int32_t res = snprintf(&strBuf[0], sizeof(strBuf), "%s got buffer at %p\n", filename, buffer);
    if ((res > 0) && (static_cast<size_t>(res) < sizeof(strBuf)))
    {
      debugInfo(&strBuf[0]);
    }

    if (readHandler != static_cast<ConfigReadHandler*>(NULL))
    {
      readHandler(filename, handle, buffer);
    }
    else
    {
      ATC::aosHalt(__FILE__, __LINE__, "No read callback was set");
    }

#ifndef _DISPATCHER
    static uint32_t endCp = 0U; // Must be initialized to 0
    vfwVisitCheckPoint(&endCp, "CFG_nvshReadCallback_end");
#endif
  }

  /******************************************************************************
  * nvshErrorCallback
  ******************************************************************************/
  void ConfigFile::nvshErrorCallback(const VFW_NvshHandle handle, const VFW_NvshError code)
  {
#ifndef _DISPATCHER
    static uint32_t beginCp = 0U; // Must be initialized to 0
    vfwVisitCheckPoint(&beginCp, "CFG_nvshErrorCallback_begin");
#endif

    if (code != VFW_NVSH_ERROR_NO_ERROR)
    {
      const char_t* const filename = vfwNvshGetFilename(handle);
      char_t strBuf[256];

      const int32_t res = snprintf(&strBuf[0], sizeof(strBuf), "NVSH ERROR! File: %s, Code: %d", filename, code);
      if ((res > 0) && (static_cast<size_t>(res) < sizeof(strBuf)))
      {
        ATC::aosHalt(__FILE__, __LINE__, &strBuf[0]);
      }
      else
      {
        ATC::aosHalt(__FILE__, __LINE__, "NVSH ERROR!");
      }
    }

#ifndef _DISPATCHER
    static uint32_t endCp = 0U; // Must be initialized to 0
    vfwVisitCheckPoint(&endCp, "CFG_nvshErrorCallback_end");
#endif
  }


  /******************************************************************************
  * VFW_NvshConsistencyHandler
  ******************************************************************************/
  void ConfigFile::nvshConsistencyHandler(const VFW_NvshHandle handle, const VFW_NvshConsistencyState conState)
  {
#ifndef _DISPATCHER
    static uint32_t beginCp = 0U; // Must be initialized to 0
    vfwVisitCheckPoint(&beginCp, "CFG_nvshConsistencyHandler_begin");
#endif

    const char_t* const filename = vfwNvshGetFilename(handle);
    char_t strBuf[256];

    const int32_t res = snprintf(&strBuf[0], sizeof(strBuf), "NVSH File: %s, Consistency State: %d\n", filename, conState);
    if ((res > 0) && (static_cast<size_t>(res) < sizeof(strBuf)))
    {
      debugInfo(&strBuf[0]);
    }

#ifndef _DISPATCHER
    static uint32_t endCp = 0U; // Must be initialized to 0
    vfwVisitCheckPoint(&endCp, "CFG_nvshConsistencyHandler_end");
#endif
  }

#ifndef _DISPATCHER

  /******************************************************************************
  * getWriteCrossCompareMaxSize
  ******************************************************************************/
  uint32_t ConfigFile::getWriteCrossCompareMaxSize() const
  {
    // The size of the data written in writeCrossCompare(), see below
    return 5U;
  }

  /******************************************************************************
  * writeCrossCompare
  ******************************************************************************/
  void ConfigFile::writeCrossCompare(VFW_Buffer * const buffer) const
  {
    vfwPutU8(buffer, isReadonly ? 255U : 0U);
    vfwPutU8(buffer, expectedMajorVersion);
    vfwPutU8(buffer, expectedMinorVersion);
    vfwPutU8(buffer, foundMajorVersion);
    vfwPutU8(buffer, foundMinorVersion);
  }

  /******************************************************************************
  * writeCrossCompareVersion
  ******************************************************************************/
  void ConfigFile::writeCrossCompareVersion(VFW_Buffer* const buffer) const
  {
    vfwPutU8(buffer, expectedMajorVersion);
    vfwPutU8(buffer, expectedMinorVersion);
  }

  /******************************************************************************
  * crossCompareVersion
  ******************************************************************************/
  bool ConfigFile::crossCompareVersion(VFW_Buffer* const buffer) const
  {
    const uint8_t majorVersion = vfwGetU8(buffer);
    const uint8_t minorVersion = vfwGetU8(buffer);

    return verifyVersion(majorVersion, minorVersion);
  }

#endif

  /******************************************************************************
  * readHandler
  ******************************************************************************/
  ConfigReadHandler* ConfigFile::readHandler = static_cast<ConfigReadHandler*>(NULL);
}
