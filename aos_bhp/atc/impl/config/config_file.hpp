#ifndef ConfigFile_hpp
#define ConfigFile_hpp
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
#include <vfw_nvsh.h>
#include "atc_base.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATC
{
  /**
  * Reads configuration parameters from the NVSH buffer.
  *
  * @param[in] filename The name of the configuration file
  * @param[in] buffer   The VFW buffer from which the parameters are to be read
  */
  typedef void ConfigReadHandler(const char_t* const filename, VFW_NvshHandle const handle, VFW_Buffer* const buffer);

  /**
  * Timeout in milliseconds for read back requests, i.e.how often a read back should be performed for this handle.
  * The SW application shall call these routines once per hour.
  */
  static const uint32_t readbackTimeOut =  3600000U;  // 1 hour in milliseconds

  /**
  * ConfigFile manages reading and writing parameters from/to a given NVSH config parameter file.
  *
  * Apart from instantiating ConfigFile objects, the user of this class must
  * also implement a ConfigReadHandler callback and install it by calling @ref setReadHandler.
  */
  class ConfigFile
  {
  public:

    /**
    * Constructor.
    *
    * Apart from instantiating ConfigFile objects, the user of this class must
    * also implement a ConfigReadHandler callback and install it by calling @ref setReadHandler.
    *
    * @param[in] size            The maximum amount of data that will be in the associated NVSH file in bytes.
    * @param[in] readOnlyParam   Used for read/write access control for parameters
    * @param[in] filename        A string identifying the file associated with this area. No file path, just a filename
    * @param[in] uniqueId        A string uniquely identifying the file and its (or a compatible) version
    * @param[in] responseTimeout Timeout in milliseconds for read and write responses from NVSH. If the timeout expires, the system halts
    * @param[in] majorVersion    The expected major version number
    * @param[in] minorVersion    The expected minor version number
    */
    ConfigFile(const uint32_t size, const bool readOnlyParam, const char_t* const filename,
      const char_t* const uniqueId, const uint32_t responseTimeout, const uint8_t majorVersion, const uint8_t minorVersion);

    /**
    * Sets the callback to be called when parameters have been read.
    *
    * @param[in] configReadHandler The read callback
    */
    static void setReadHandler(ConfigReadHandler* const configReadHandler);

    /**
    * Initiates the reading of parameters from the NVSH file. This causes the read handler
    * (supplied to the constructor) to be called, one for each parameter read.
    */
    void readParameters() const;

    /**
    * Writes the given parameter data to the NVSH file.
    *
    * @param[in] buffer  Buffer containing the configuration version plus one or more parameters
    */
    void writeParameters(VFW_Buffer* const buffer);

    /**
    * getter for nvshHandle
    *
    * @return    The NVSH file handle aquired with vfwNvshAllocate on setup
    */
    VFW_NvshHandle getNvshHandle(void);

    /**
    * Returns the expected major version number
    *
    * @return the expected major version number
    */
    uint8_t getExpectedMajorVersion() const;

    /**
    * Returns the expected minor version number
    *
    * @return the expected minor version number
    */
    uint8_t getExpectedMinorVersion() const;

    /**
    * Reads the config version from the given buffer and checks if it's valid.
    *
    * @param[in] buffer The buffer containing the config memory area
    *
    * @return true if the config version matches the expected version
    */
    bool readAndVerifyVersion(VFW_Buffer* const buffer);

    /**
    * Checks if the given config version is valid.
    *
    * @param[in] majorVersion The major version to verify
    * @param[in] minorVersion The minor version to verify
    *
    * @return true if the config version matches the expected version
    */
    bool verifyVersion(const uint8_t majorVersion, const uint8_t minorVersion) const;

#ifndef _DISPATCHER

    /**
    * The maximum size for the cross compare data
    *
    * @return Returns the maximum data size for this item
    */
    uint32_t getWriteCrossCompareMaxSize() const;

    /**
    * Write all attributes to cross compare in the provided buffer
    *
    * @param[in] buffer        The cross compare buffer to write the attributes in
    */
    void writeCrossCompare(VFW_Buffer* const buffer) const;

    /**
    * Writes the configuration version for cross comparison.
    *
    * @param[in] buffer   Buffer to store the cross compare data in
    */
    void writeCrossCompareVersion(VFW_Buffer* const buffer) const;

    /**
    * Cross compares the configuration version.
    *
    * @param[in] buffer   Buffer to read the cross compare data from
    *
    * @return True if the versions match
    */
    bool crossCompareVersion(VFW_Buffer* const buffer) const;

#endif

  protected:

  private:

    /**
    * Default constructor (disabled)
    */
    ConfigFile();

    /**
    * Callback function for when items have been read from nvs
    * This method needs to be static since it will be used as a callback function for NVSH.
    *
    * @param[in] handle   Handle to the read NVSH file
    * @param[in] buffer   The data read from the NVSH file
    */
    static void nvshReadCallback(const VFW_NvshHandle handle, VFW_Buffer* const buffer);

    /**
    * Callback function for errors from nvs
    * This method needs to be static since it will be used as a callback function for NVSH.
    *
    * @param[in] handle   Handle to the read NVSH file
    * @param[in] code     The code for the error that occurred
    */
    static void nvshErrorCallback(const VFW_NvshHandle handle, const VFW_NvshError code);

    /**
    * Callback function for consistency
    * This method needs to be static since it will be used as a callback function for NVSH.
    *
    * @param[in] handle       Handle to the read NVSH file
    * @param[in] conState     The consistency state of the NVSH file
    */
    static void nvshConsistencyHandler(const VFW_NvshHandle handle, const VFW_NvshConsistencyState conState);

    /**
    * Access control to allow/disallow change of parameters.
    * true = Read, false = Read/Write
    */
    bool isReadonly;

    /**
    * The expected major version number
    */
    uint8_t expectedMajorVersion;

    /**
    * The expected minor version number
    */
    uint8_t expectedMinorVersion;

    /**
    * The found major version number (set by @ref readAndVerifyVersion)
    */
    uint8_t foundMajorVersion;

    /**
    * The found minor version number (set by @ref readAndVerifyVersion)
    */
    uint8_t foundMinorVersion;

    /**
    * The NVSH file handle associated with the configuration file
    */
    VFW_NvshHandle nvshHandle;

    /**
    * The callback which will be called when a parameter file has been read
    */
    static ConfigReadHandler* readHandler;
  };
}
#endif
