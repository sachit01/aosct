#ifndef BaseConfigItem_hpp
#define BaseConfigItem_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Declaration for BaseConfigItem class, used as foundation for all config items managed by AOS.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-06-10    jeneman     Created
* 2016-07-07    jeneman     Removed things related to writing to NVS
* 2017-01-12    saprasad    Added port number and sendcycle for Analyzer IF in Adaptation
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "config_file.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATC
{

  /**
  * The type definition for config item id numbers.
  *
  * The id numbers shall be allocated in the following ranges:
  *   0       - Undefined/Illegal
  *   1-99    - ATC items
  *   100-299 - ATP Core items
  *   300-399 - Adaptation items
  *   400-499 - ATO items
  *   500-599 - Dispatcher items
  */
  typedef uint16_t ConfigIdType;

  /**
  * Declaration of Config ID's
  */
  static const ConfigIdType njruIpId = 1U; //!< NJRU IP address
  static const ConfigIdType njruPortId = 2U; //!< NJRU Port
  static const ConfigIdType bdsPortId = 4U; //!< BDS Port ID
  static const ConfigIdType consolePortId = 5U; //!< Console Port ID
  static const ConfigIdType njruLevelId = 6U; //!< N-JRU ID
  static const ConfigIdType bdsLevelId = 7U; //!< BDS ID  
  static const ConfigIdType ruIpId = 8U; //!< RU IP address
  static const ConfigIdType ruPortId = 9U; //!< RU Port
  static const ConfigIdType ruLogDynValuePeriodId = 10U; //!< Period for logging dynamic values
  static const ConfigIdType ruLogValueDiffId = 11U; //!< Min. diff. for logging analog inputs

  /**
  * Size of the buffer when values of config items need to be converted to a string
  */
  static const uint8_t valBufferSize = 50U; //!< BufferSize to store config value as string (including the terminating null)
  //lint -esym(551,ATC::valBufferSize) Lint is wrong, this constant *is* used

  /**
  * Buffer to store config value as string (including the terminating null)
  */
  struct ValueBuffer {
    char_t buf[valBufferSize]; //!< Buffer to store config value as string (including the terminating null)
  };

  /** Base class (abstract) for all config items
  *
  * The class BaseConfigItem is an abstract class representing the common parts of a generic
  * config item. The class is used to allow a common handling of config items such as fetching
  * the item value from the VFW_Buffer.
  */
  class BaseConfigItem
  {
  public:

    /**
    * Enumerates the data types available for configuration parameters.
    */
    enum ItemDatatype {
      ItemDatatypeU8,
      ItemDatatypeI8,
      ItemDatatypeU16,
      ItemDatatypeI16,
      ItemDatatypeU32,
      ItemDatatypeI32,
      ItemDatatypeU64,
      ItemDatatypeI64,
      ItemDatatypeText,
      ItemDatatypeBool,
      ItemDatatypeIpaddress
    };

    /**
    * Constructor (explicit)
    *
    * @param[in] id          The ID of this item
    * @param[in] itemName    The name of the item, length of string has to be <= maxConfigNameLength
    * @param[in] desc        A text describing the item
    * @param[in] unit        A text telling which unit the value of this item has (cm, kg, etc)
    * @param[in] configFile  Pointer to a ConfigFile (Common, Maintenance etc)
    * @param[in] datatype    The data type of this item
    */
    explicit BaseConfigItem(const ConfigIdType id, const char_t * const itemName, const char_t * const desc,
      const char_t * const unit, ConfigFile * const configFile, const ItemDatatype datatype);

    /**
    * Destructor (default)
    *
    */
    virtual ~BaseConfigItem();

    /**
    * Get the value from the VFW_Buffer and store as value in the item itself.
    *
    * Abstract method to retrieve the correct type of value from the buffer for each subclass.
    * @param[in] buf        A buffer to get the value from
    *
    * @return true if the value can be parsed and the value fits within the boundaries, false otherwise.
    */
    virtual bool readValueFromBuffer(VFW_Buffer * const buf) = 0;

    /**
    * Parse supplied string and use it as new value for this item
    *
    * Abstract method to parse to the correct type of value for each subclass.
    *
    * @param[in] str       The string to be parsed
    *
    * @return true if the string can be parsed and the value fits within the boundaries, false otherwise.
    */
    virtual bool parseStringToValue(const char_t * const str) = 0;

    /**
    * Get the value and boundaries from the item as strings
    *
    * Abstract method to convert the value and boundaries to text for each subclass.
    *
    * @param[out] valbuf     Pointer to where to store the textual representation of the value
    * @param[out] maxbuf     Pointer to where to store the textual representation of the max value
    * @param[out] minbuf     Pointer to where to store the textual representation of the min value
    */
    virtual void getValueStrings(ValueBuffer& valbuf, ValueBuffer& maxbuf, ValueBuffer& minbuf) const = 0;

    /**
    * Get the value from the item as string
    *
    * Abstract method to convert the value to text for each subclass.
    *
    * @param[out] valbuf     Pointer to where to store the textual representation of the value
    */
    virtual void getValueString(ValueBuffer& valbuf) const = 0;


    /*************************/
    /* Data access functions */
    /*************************/

    /**
    * Get config item ID
    *
    * @return config item id
    */
    ConfigIdType getConfigItemId() const;

    /**
    * Get the name
    *
    * @return Pointer to the text
    */
    const char_t* getName() const;

    /**
    * Get the Global name
    *
    * @return Pointer to the Global Name text
    */
    const char_t* getGlobalName() const;

    /**
    * Get the descriptive text
    *
    * @return Pointer to the text
    */
    const char_t* getDescriptiveText() const;

    /**
    * Get the unit string
    *
    * @return Pointer to the text
    */
    const char_t* getUnitString() const;

    /**
    * Access function to set the paramRead flag
    */
    void setReadFlagConfigID(void);
    
    /**
    * Access function to read the paramRead flag
    */
    bool isReadFlagConfigID() const;

    /**
    * Get the memory area
    *
    * @return pointer to the memory area
    */
    ConfigFile* getConfigFile();

    /**
    * Get the memory area
    *
    * @return pointer to the memory area
    */
    const ConfigFile* getConfigFile() const;

    /**
    * Returns the data type of this configuration parameter.
    *
    * @return the data type of this configuration parameter.
    */
    ItemDatatype getDatatype() const;

    /**
    * set the global name for Configuration parameters Name.
    *
    * @param[in]  globalNameToSet    Global Name to be set
    *
    * @return true if the global name is set successfully
    */
    bool setGlobalName(const char_t* const globalNameToSet);

#ifndef _DISPATCHER

    /**
    * Add all attributes to cross compare
    *
    * @param[in] buffer        The cross compare buffer to get store the attributes in
    */
    void writeCrossCompare(VFW_Buffer* const buffer) const;

    /**
    * The maximum size for the cross compare data
    *
    * @return Returns the maximum data size for this item
    */
    uint32_t getWriteCrossCompareMaxSize() const;

#endif

  protected:

    /**
    * Copies a string to a ValueBuffer.
    *
    * Truncates the string to one less than the length of ValueBuffer.buf.
    * The copied string will be NUL-terminated.
    *
    * @param buffer  the buffer to copy to
    * @param string  the string to copy from
    */
    static void copyTo(ValueBuffer& buffer, const char_t* const string);

    /**
    * Abstract method to add attributes of each subclass.
    * The method should call initBaseCrossCompare()
    * @param[in] buffer        The cross compare buffer to get store the attributes in
    */
    virtual void writeCrossCompareSubClass(VFW_Buffer* const buffer) const = 0;

    /**
    * Method to write a bool
    * @param[in] buffer        The cross compare buffer to get store the attributes in
    * @param[in] value         Boolean value to write
    */
    void writeCrossCompareBool(VFW_Buffer* const buffer, const bool value) const;

  private:
    /**
    * Constructor (default)
    *
    */
    BaseConfigItem();

    /**
    * The Id of this item
    *
    */
    const ConfigIdType configItemId;

    /**
    * Maximum Length of the Configuration parameter Name allowed 
    *
    */
    static const uint8_t maxConfigNameLength = 20U;

    /**
    * The name of the item
    *
    */
    char_t nameString[maxConfigNameLength+1U];

    /**
    * The name of the item when accessed from TCC
    *
    */
    char_t globalName[maxConfigNameLength + 1U];

    /**
    * A text describing this item
    *
    */
    const char_t* descriptiveText;

    /**
    * A text telling which unit the value of this item has (cm, kg, etc)
    *
    */
    const char_t* unitString;

    /**
    * Flag denoting if the config item is read
    *
    */
    bool paramRead;

    /**
    * The configuration file that the item belongs to
    */
    ConfigFile* cfgFile;

    /**
    * Item data type
    *
    */
    const ItemDatatype itemDatatype;
  };
}

#endif
