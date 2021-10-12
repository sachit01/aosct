#ifndef AbstractConfigBase_hpp
#define AbstractConfigBase_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This abstract class defines the Config functionality.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-06-03    jeneman     Created
* 2016-07-07    jeneman     Removed things related to writing to NVS
* 2016-09-15    nsyed       Added individual access functions to the config params
* 2017-02-02    spandita    Added Ipaddress Add config item member function
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_base.hpp"
#include "config_file.hpp"
#include "bool_config_item.hpp"
#include "int8_config_item.hpp"
#include "int16_config_item.hpp"
#include "int32_config_item.hpp"
#include "int64_config_item.hpp"
#include "uint8_config_item.hpp"
#include "uint16_config_item.hpp"
#include "uint32_config_item.hpp"
#include "uint64_config_item.hpp"
#include "string_config_item.hpp"
#include "ipaddress_config_item.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATC
{
  /**
  * The lowest allowed ID of a configItem.
  */
  static const ConfigIdType firstConfigItem = 1U;

  /**
  * The maximum number of configItems.
  */
  static const ConfigIdType maxConfigItems = 600U;
  //lint -esym(551,ATC::maxConfigItems) Lint is wrong, this constant *is* used

  class AbstractConfigBase;

  /**
  * Static variable to store the single instance of AbstractConfigBase
  *
  * Variable shall be setup during construction of the single instance used within ATP.
  * The variable is returned by @ref basePtr and used by the core ATP logic to access
  * adaptation objects through the core class.
  *
  * Note: During construction the variable shall be checked to guarantee that only
  *       one instance is created. Should the variable be set to non-zero the execution shall
  *       be immediately interrupted and a safe state issued.
  */
  static AbstractConfigBase* coreAbstractConfigBasePtr = static_cast<AbstractConfigBase*>(NULL);

  /**
  * The class AbstractConfigBase implements the interface defined by the ProcComponent class,
  * and provides the base functionality for reading configuration parameters from NVS and providing them to other components.
  *
  */
  class AbstractConfigBase : public ProcComponent
  {
  public:

    /**
    * Implements the virtual init function.
    *
    * This init function needs to be finished before any other components can start their
    * initialization, since they are dependent on the parameters read in to the system during
    * the initialization of Config.
    * This init function will need to be run multiple times to go through all states before
    * reaching READY, since the reading of configuration files is an asynchronous process.
    *
    * @return true when initialization completed
    */
    virtual bool init();

    /**
    * Implements the virtual run function.
    */
    virtual void run();

    /**
    * Get base instance pointer
    *
    * @return Pointer to single instance base object.
    */
    static AbstractConfigBase* basePtr();

    /**
    * Get a config item based on ID
    *
    * @param[in] id  The id of the requested config item
    *
    * @return Pointer to the config item
    */
    BaseConfigItem* getConfigItem(const ConfigIdType id);

    /**
    * Get a config item based on ID
    *
    * @param[in] id  The id of the requested config item
    *
    * @return Pointer to the config item
    */
    const BaseConfigItem* getConfigItem(const ConfigIdType id) const;

    /**
    * Get the NJRU IP address
    * This in turn calls the relevant getconfig function
    *
    * @return NJRU IP address
    */
    const char_t* getNjruIp() const;

    /**
    * Get the RU IP address
    * This in turn calls the relevant getconfig function
    *
    * @return RU IP address
    */
    const char_t* getRuIp() const;

    /**
    * Get the NJRU Port
    * This in turn calls the relevant getconfig function
    *
    * @return Njru Port
    */
    uint16_t getNjruPort() const;

    /**
    * Get the RU Port
    * This in turn calls the relevant getconfig function
    *
    * @return RU Port
    */
    uint16_t getRuPort() const;

    /**
    * Get the BDS Port
    * This in turn calls the relevant getconfig function
    *
    * @return BDS Port
    */
    uint16_t getBdsPort() const;

    /**
    * Get the Console Port
    * This in turn calls the relevant getconfig function
    *
    * @return Console Port
    */
    uint16_t getConsolePort() const;

    /**
    * Get the BDS log-level
    * This in turn calls the relevant getconfig function
    *
    * @return BDS log-level
    */
    uint8_t getBdsLevel() const;

    /**
    * Get the NJRU log-level
    * This in turn calls the relevant getconfig function
    *
    * @return NJRU log-level
    */
    uint8_t getNjruLevel() const;

    /**
    * Get the period time for logging dynamic values to RU.
    * This in turn calls the relevant getconfig function
    *
    * @return The period time in seconds
    */
    uint8_t getRuLogDynValuePeriod() const;

    /**
    * Get the minimum difference for logging analog inputs to RU.
    * This in turn calls the relevant getconfig function
    *
    * @return The minimum difference for logging analog inputs
    */
    uint16_t getRuLogValueDiff() const;

    /**
    * Write a new value to a config item based on ID
    *
    * String representation of value will if possible be converted to
    * the datatype of the item.
    * If the string cannot be converted, or if the converted value is out of bounds,
    * the method will return false and the value of the item will not be changed.
    *
    * @param[in] id   The id of the config item to update
    * @param[in] str  Pointer to the textual representation of the value
    *
    * @return true if the config item was of correct type
    */
    bool putConfigString(const ConfigIdType id, const char_t * const str);

    /**
    * Interface to call different level of Console Command
    *
    * @param[in] argc  Number of arguments in the argument array argv
    * @param[in] argv  Arguments array
    *
    * @return true if the Call is successful.
    */
    virtual bool consoleCall(const uint32_t argc, const ConsoleArguments argv);
    
#ifdef _SIL
    /**
    * Get info of item (if one with supplied id exists) to fetch it from ini-file and write to buf
    */
    bool getItemInfoForVfwSim(const ConfigIdType id, const char_t* & itemName,
      BaseConfigItem::ItemDatatype & itemType, ConfigFile* & configFile);
#endif

#ifndef _DISPATCHER

    /**
    * write cross compare data.
    * @param[in] buffer   Buffer to store the cross compare data in
    */
    void writeCrossCompare(VFW_Buffer* const buffer) const;

    /**
    * The maximum size for the cross compare data
    *
    * @return Returns the maximum data size for this item
    */
    uint32_t getWriteCrossCompareMaxSize() const;

    /**
    * Writes the configuration versions for cross comparison.
    *
    * @param[in] buffer   Buffer to store the cross compare data in
    */
    void writeCrossCompareVersions(VFW_Buffer* const buffer) const;

    /**
    * Cross compares the configuration versions.
    *
    * @param[in] buffer   Buffer to read the cross compare data from
    *
    * @return True if the versions match
    */
    bool crossCompareVersions(VFW_Buffer* const buffer) const;

#endif

  protected:

    /**
    * Error reporting while reading the configuration parameters.
    *
    * @param[in] filepath The complete path of the file with the source-code where the error was reported
    * @param[in] line     The line where the error was reported
    * @param[in] str      The string describing the error to be reported
    */
    static void reportConfigError(const char_t* const filepath, int32_t const line, const char_t * const str);

    /**
    * Function to write any value (uint32_t) in Non-Volatile Storage
    *
    * @param [in] valueToStoreInNVS current value to write in NVSH
    * @param [in] id configuration id for which data has to be written in NVS
    *
    * @return true, if successfully written the time in NVS
    */
    bool setRunTimeConfigValue(const ConfigIdType id, const uint32_t valueToStoreInNVS);

    /**
    * The configuration file where vehicle Common parameters are stored.
    * This variable needs to be static since it will be accessed by a callback function for NVSH.
    *
    */
    static ConfigFile* configFileCommon;

    /**
    * The configuration file where vehicle Runtime type parameters are stored.
    * This variable needs to be static since it will be accessed by a callback function for NVSH.
    *
    */
    static ConfigFile* configFileRuntime;

    /**
    * The configuration file where maintenance parameters are stored.
    * This variable needs to be static since it will be accessed by a callback function for NVSH.
    *
    */
    static ConfigFile* configFileMaint;

    /**
    * The configuration file where Instancetype Specific parameters are stored.
    * This variable needs to be static since it will be accessed by a callback function for NVSH.
    *
    */
    static ConfigFile* configFileInstance;

    /**
    * The configuration file where Vehicletype Specific parameters are stored.
    * This variable needs to be static since it will be accessed by a callback function for NVSH.
    *
    */
    static ConfigFile* configFileType;

    /**
    * The configuration file where dispatcher specific parameters are stored.
    * This variable needs to be static since it will be accessed by a callback function for NVSH.
    */
    static ConfigFile* configFileDisp;

    /**
    * Constructor
    */
    AbstractConfigBase();

    /**
    * Add a config item to the collection (only done during init)
    *
    * @param[in] pItem  Pointer to the item to be added
    *
    */
    //lint -sem(ATC::AbstractConfigBase::addConfigItem,custodial(1))
    void addConfigItem(BaseConfigItem * const pItem);

    /**
    * Creates a Uint8ConfigItem and adds it to the vector of config items.
    *
    * @param[in] id         The ID of this item
    * @param[in] itemName   The name of the item
    * @param[in] desc       A text describing the item
    * @param[in] unit       A text telling which unit the value of this item has (cm, kg, etc)
    * @param[in] minimum    The lower limit for the value of this item
    * @param[in] maximum    The upper limit for the value of this item
    * @param[in] defVal     The default value of this item
    * @param[in] configFile The configuration file that this item belongs to
    */
    void addUint8ConfigItem(const ConfigIdType id, const char_t * const itemName, const char_t * const desc,
      const char_t * const unit, const uint8_t minimum, const uint8_t maximum, const uint8_t defVal, ConfigFile * const configFile);

    /**
    * Creates a Int8ConfigItem and adds it to the vector of config items.
    *
    * @param[in] id         The ID of this item
    * @param[in] itemName   The name of the item
    * @param[in] desc       A text describing the item
    * @param[in] unit       A text telling which unit the value of this item has (cm, kg, etc)
    * @param[in] minimum    The lower limit for the value of this item
    * @param[in] maximum    The upper limit for the value of this item
    * @param[in] defVal     The default value of this item
    * @param[in] configFile The configuration file that this item belongs to
    */
    void addInt8ConfigItem(const ConfigIdType id, const char_t * const itemName, const char_t * const desc,
      const char_t * const unit, const int8_t minimum, const int8_t maximum, const int8_t defVal, ConfigFile * const configFile);

    /**
    * Creates a Uint16ConfigItem and adds it to the vector of config items.
    *
    * @param[in] id         The ID of this item
    * @param[in] itemName   The name of the item
    * @param[in] desc       A text describing the item
    * @param[in] unit       A text telling which unit the value of this item has (cm, kg, etc)
    * @param[in] minimum    The lower limit for the value of this item
    * @param[in] maximum    The upper limit for the value of this item
    * @param[in] defVal     The default value of this item
    * @param[in] configFile The configuration file that this item belongs to
    */
    void addUint16ConfigItem(const ConfigIdType id, const char_t * const itemName, const char_t * const desc,
      const char_t * const unit, const uint16_t minimum, const uint16_t maximum, const uint16_t defVal, ConfigFile * const configFile);

    /**
    * Creates a Int16ConfigItem and adds it to the vector of config items.
    *
    * @param[in] id         The ID of this item
    * @param[in] itemName   The name of the item
    * @param[in] desc       A text describing the item
    * @param[in] unit       A text telling which unit the value of this item has (cm, kg, etc)
    * @param[in] minimum    The lower limit for the value of this item
    * @param[in] maximum    The upper limit for the value of this item
    * @param[in] defVal     The default value of this item
    * @param[in] configFile The configuration file that this item belongs to
    */
    void addInt16ConfigItem(const ConfigIdType id, const char_t * const itemName, const char_t * const desc,
      const char_t * const unit, const int16_t minimum, const int16_t maximum, const int16_t defVal, ConfigFile * const configFile);

    /**
    * Creates a Uint32ConfigItem and adds it to the vector of config items.
    *
    * @param[in] id         The ID of this item
    * @param[in] itemName   The name of the item
    * @param[in] desc       A text describing the item
    * @param[in] unit       A text telling which unit the value of this item has (cm, kg, etc)
    * @param[in] minimum    The lower limit for the value of this item
    * @param[in] maximum    The upper limit for the value of this item
    * @param[in] defVal     The default value of this item
    * @param[in] configFile The configuration file that this item belongs to
    */
    void addUint32ConfigItem(const ConfigIdType id, const char_t * const itemName, const char_t * const desc,
      const char_t * const unit, const uint32_t minimum, const uint32_t maximum, const uint32_t defVal, ConfigFile * const configFile);

    /**
    * Creates a Int32ConfigItem and adds it to the vector of config items.
    *
    * @param[in] id         The ID of this item
    * @param[in] itemName   The name of the item
    * @param[in] desc       A text describing the item
    * @param[in] unit       A text telling which unit the value of this item has (cm, kg, etc)
    * @param[in] minimum    The lower limit for the value of this item
    * @param[in] maximum    The upper limit for the value of this item
    * @param[in] defVal     The default value of this item
    * @param[in] configFile The configuration file that this item belongs to
    */
    void addInt32ConfigItem(const ConfigIdType id, const char_t * const itemName, const char_t * const desc,
      const char_t * const unit, const int32_t minimum, const int32_t maximum, const int32_t defVal, ConfigFile * const configFile);

    /**
    * Creates a Uint64ConfigItem and adds it to the vector of config items.
    *
    * @param[in] id         The ID of this item
    * @param[in] itemName   The name of the item
    * @param[in] desc       A text describing the item
    * @param[in] unit       A text telling which unit the value of this item has (cm, kg, etc)
    * @param[in] minimum    The lower limit for the value of this item
    * @param[in] maximum    The upper limit for the value of this item
    * @param[in] defVal     The default value of this item
    * @param[in] configFile The configuration file that this item belongs to
    */
    void addUint64ConfigItem(const ConfigIdType id, const char_t * const itemName, const char_t * const desc,
      const char_t * const unit, const uint64_t minimum, const uint64_t maximum, const uint64_t defVal, ConfigFile * const configFile);

    /**
    * Creates a Int64ConfigItem and adds it to the vector of config items.
    *
    * @param[in] id         The ID of this item
    * @param[in] itemName   The name of the item
    * @param[in] desc       A text describing the item
    * @param[in] unit       A text telling which unit the value of this item has (cm, kg, etc)
    * @param[in] minimum    The lower limit for the value of this item
    * @param[in] maximum    The upper limit for the value of this item
    * @param[in] defVal     The default value of this item
    * @param[in] configFile The configuration file that this item belongs to
    */
    void addInt64ConfigItem(const ConfigIdType id, const char_t * const itemName, const char_t * const desc,
      const char_t * const unit, const int64_t minimum, const int64_t maximum, const int64_t defVal, ConfigFile * const configFile);

    /**
    * Creates a StringConfigItem and adds it to the vector of config items.
    *
    * @param[in] id         The ID of this item
    * @param[in] itemName   The name of the item
    * @param[in] desc       A text describing the item
    * @param[in] unit       A text telling which unit the value of this item has (cm, kg, etc)
    * @param[in] minimum    Minimum size of allowed text for this item
    * @param[in] maximum    Maximum size of allowed text for this item
    * @param[in] defVal     The default value of this item
    * @param[in] configFile The configuration file that this item belongs to
    */
    void addStringConfigItem(const ConfigIdType id, const char_t * const itemName, const char_t * const desc,
      const char_t * const unit, const uint8_t minimum, const uint8_t maximum, const char_t * const defVal, ConfigFile * const configFile);

    /**
    * Creates a BoolConfigItem and adds it to the vector of config items.
    *
    * @param[in] id         The ID of this item
    * @param[in] itemName   The name of the item
    * @param[in] desc       A text describing the item
    * @param[in] unit       A text telling which unit the value of this item has (cm, kg, etc)
    * @param[in] defVal     The default value of this item
    * @param[in] configFile The configuration file that this item belongs to
    */
    void addBoolConfigItem(const ConfigIdType id, const char_t * const itemName, const char_t * const desc,
      const char_t * const unit, const bool defVal, ConfigFile * const configFile);


    /**
    * Creates addIPaddrConfigItem and adds it to the vector of config items.
    *
    * @param[in] id         The ID of this item
    * @param[in] itemName   The name of the item
    * @param[in] desc       A text describing the item
    * @param[in] unit       A text telling which unit the value of this item has (cm, kg, etc)
    * @param[in] defVal     The default value of this item
    * @param[in] configFile The configuration file that this item belongs to
    */
    void addIPaddrConfigItem(const ConfigIdType id, const char_t * const itemName, const char_t * const desc,
      const char_t * const unit, const char_t * const defVal, ConfigFile * const configFile);

    /**
    * Get the uint8_t value of a config item based on ID
    *
    * If the datatype of the value of the requested item is not uint8_t
    * no value will be set in val, and the method will return false.
    *
    * @param[in]  id   The id of the requested config item
    * @param[out] val  Reference to where to store the retrieved value
    *
    * @return true if the config item was of correct type
    */
    bool getConfig(const ConfigIdType id, uint8_t & val) const;

    /**
    * Get the int8_t value of a config item based on ID
    *
    * If the datatype of the value of the requested item is not int8_t
    * no value will be set in val, and the method will return false.
    *
    * @param[in]  id   The id of the requested config item
    * @param[out] val  Reference to where to store the retrieved value
    *
    * @return true if the config item was of correct type
    */
    bool getConfig(const ConfigIdType id, int8_t & val) const;

    /**
    * Get the uint16_t value of a config item based on ID
    *
    * If the datatype of the value of the requested item is not uint16_t
    * no value will be set in val, and the method will return false.
    *
    * @param[in]  id   The id of the requested config item
    * @param[out] val  Reference to where to store the retrieved value
    *
    * @return true if the config item was of correct type
    */
    bool getConfig(const ConfigIdType id, uint16_t & val) const;

    /**
    * Get the int16_t value of a config item based on ID
    *
    * If the datatype of the value of the requested item is not int16_t
    * no value will be set in val, and the method will return false.
    *
    * @param[in]  id   The id of the requested config item
    * @param[out] val  Reference to where to store the retrieved value
    *
    * @return true if the config item was of correct type
    */
    bool getConfig(const ConfigIdType id, int16_t & val) const;

    /**
    * Get the uint32_t value of a config item based on ID
    *
    * If the datatype of the value of the requested item is not uint32_t
    * no value will be set in val, and the method will return false.
    *
    * @param[in]  id   The id of the requested config item
    * @param[out] val  Reference to where to store the retrieved value
    *
    * @return true if the config item was of correct type
    */
    bool getConfig(const ConfigIdType id, uint32_t & val) const;

    /**
    * Get the int32_t value of a config item based on ID
    *
    * If the datatype of the value of the requested item is not int32_t
    * no value will be set in val, and the method will return false.
    *
    * @param[in]  id   The id of the requested config item
    * @param[out] val  Reference to where to store the retrieved value
    *
    * @return true if the config item was of correct type
    */
    bool getConfig(const ConfigIdType id, int32_t & val) const;

    /**
    * Get the uint64_t value of a config item based on ID
    *
    * If the datatype of the value of the requested item is not uint64_t
    * no value will be set in val, and the method will return false.
    *
    * @param[in]  id   The id of the requested config item
    * @param[out] val  Reference to where to store the retrieved value
    *
    * @return true if the config item was of correct type
    */
    bool getConfig(const ConfigIdType id, uint64_t & val) const;

    /**
    * Get the int64_t value of a config item based on ID
    *
    * If the datatype of the value of the requested item is not uint64_t
    * no value will be set in val, and the method will return false.
    *
    * @param[in]  id   The id of the requested config item
    * @param[out] val  Reference to where to store the retrieved value
    *
    * @return true if the config item was of correct type
    */
    bool getConfig(const ConfigIdType id, int64_t & val) const;

    /**
    * Get the string (char_t *) value of a config item based on ID
    *
    * If the datatype of the value of the requested item is not char_t[]
    * no value will be set in val, and the method will return false.
    *
    * @param[in]  id   The id of the requested config item
    * @param[out] val  Reference to the pointer that should point to the string
    *
    * @return true if the config item was of correct type
    */
    bool getConfig(const ConfigIdType id, const char_t* & val) const;

    /**
    * Get the boolean value of a config item based on ID
    *
    * If the datatype of the value of the requested item is not bool
    * no value will be set in val, and the method will return false.
    *
    * @param[in]  id   The id of the requested config item
    * @param[out] val  Reference to where to store the retrieved value
    *
    * @return true if the config item was of correct type
    */
    bool getConfig(const ConfigIdType id, bool & val) const;

    /**
    * Display all the config items on to the console
    */
    void displayAllConfigItems() const;

    /**
    * Display all the details of the config item requested by its ID
    *
    * @param[in]  id   The id of the requested config item
    */
    void displayConfigItem(const ConfigIdType id) const;

    /**
    * Access function to set the global name for the cross reference of the 
    * Configuration parameters Name.
    *
    * @param[in]  id               The id of the configuration item to set global name
    * @param[in]  globalName       Global Name to be set
    */
    void setGlobalNameToConfigParameter(const ConfigIdType id, const char_t* const globalName);


  private:

    /**
    * Size of buffer
    */
    static const int16_t buffSize = 400;

    /**
    * The array of ConfigItems.
    *
    */
    BaseConfigItem* configItems[maxConfigItems];

    /**
    * The states that the config component will go through during init.
    */
    enum ConfigState
    {
      ConfigStateUninitialized, //!<Initial state, this is when to setup memory areas and add items.
      ConfigStateReadingFiles,  //!<Reading from non-volatile storage, and updating config items when files are read
      ConfigStateReady         //!<All items have their values, and Config is ready to provide them to other components
    };

    /**
    * The state of Config. Changes during initialization, and when init is done it is READY.
    *
    */
    ConfigState state;

    /**
    * The number of config files that remain to be read
    */
    uint16_t numberOfConfigFilesToRead;

    /**
    * Total number of added Config Items
    */
    uint16_t numberOfConfigItems;

    /**
    * Expected number of config-parameters to read form Config-file.
    */
    uint16_t expectedNumberOfConfigItemsToReadFromFile;

    /**
    * Number of Config Items read till now
    */
    uint16_t numConfigItemsRead;

    /**
    * Array storing the Ids of config parameters added
    */
    ConfigIdType configParamIds[maxConfigItems];

    /**
    * Flag to check whether any runtime config parameter is updated
    */
    bool isAnyRunTimeConfigUpdated;

    /**
    * Saves the Run Time Configuration Parameters to Non-Volatile Storage.
    */
    void saveRunTimeParameters() const;

    /**
    * Reads the runtime configuration parameters from the NVSH buffer.
    *
    * @param[in] filename The name of the configuration file
    * @param[in] handle   The VFW handle that identifies the @ref ConfigFile instance
    * @param[in] buffer   The VFW buffer from which the parameter is to be read
    */
    static void readConfigParameters(const char_t* const filename, VFW_NvshHandle const handle, VFW_Buffer* const buffer);

    /**
    * Reads configuration parameters from the NVSH buffer.
    *
    * @param[in] handle   The VFW handle that identifies the @ref ConfigFile instance
    * @param[in] buffer   The VFW buffer from which the parameters are to be read
    * @param[in] readOnly Indicates whether the parameters are read-only
    */
    void readConfigParameters(VFW_NvshHandle const handle, VFW_Buffer* const buffer, const bool readOnly);

    /**
    * Get the information of a config item as strings, for display in console.
    *
    * If there is no item corresponding to the supplied id, the method will return false
    * and no values will be set.
    * The description and unit are already strings stored in the config item, so these only need pointers.
    * But for the value, max and min sufficiently large buffers need to be created and the pointers to those supplied to the function.
    *
    * @param[in]  id       The id of the requested config item
    * @param[out] itemName Reference to pointer which is to point at the name of the item
    * @param[out] desc     Reference to pointer which is to point at the descriptive text
    * @param[out] val      The buffer where to store the textual representation of the value
    * @param[out] unit     Reference to pointer which is to point at the unit text
    * @param[out] minimum  The buffer where to store the text representation of the minmum allowed value ("0" for string and bool items)
    * @param[out] maximum  The buffer where to store the text representation of the maximum allowed value (size for string items, "1" for bool items)
    *
    * @return true if there is an item with supplied id
    */
    bool getConfigInfo(const ConfigIdType id, const char_t* & itemName, const char_t* & desc, ValueBuffer& val,
      const char_t* & unit, ValueBuffer& minimum, ValueBuffer& maximum) const;

    /**
    * Get the information of a config item as strings, for display in console.
    *
    * If there is no item corresponding to the supplied id, the method will return false
    * and no values will be set.
    * The description and unit are already strings stored in the config item, so these only need pointers.
    * But for the value sufficiently large buffers need to be created and the pointers to those supplied to the function.
    *
    * @param[in]  id       The id of the requested config item
    * @param[out] itemName Reference to pointer which is to point at the name of the item
    * @param[out] val      The buffer where to store the textual representation of the value
    * @param[out] unit     Reference to pointer which is to point at the unit text
    *
    * @return true if there is an item with supplied id
    */
    bool getConfigInfo(const ConfigIdType id, const char_t* & itemName, ValueBuffer& val, const char_t* & unit) const;

  };

}

#endif
