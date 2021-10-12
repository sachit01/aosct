/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of BaseConfigItem
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
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "base_config_item.hpp"
#include <vfw_string.h>

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
  * Constructors
  ******************************************************************************/
  BaseConfigItem::BaseConfigItem(const ConfigIdType id, const char_t * const itemName, const char_t * const desc,
    const char_t * const unit, ConfigFile * const configFile, const ItemDatatype datatype) :
    configItemId(id),
    descriptiveText(desc),
    unitString(unit),
    paramRead(false),
    cfgFile(configFile),
    itemDatatype(datatype)
  {
    static_cast<void>(vfw_strlcpy(&nameString[0], itemName, sizeof(nameString))); // assume that strlen(itemlen) <= maxConfigNameLength
    memset(&globalName[0], 0, sizeof(globalName));
  }

  /******************************************************************************
  * Destructor
  ******************************************************************************/
  BaseConfigItem::~BaseConfigItem()
  {
    memset(&nameString[0], 0, sizeof(nameString));
    memset(&globalName[0], 0, sizeof(globalName));
    descriptiveText = static_cast<char_t*>(NULL);
    unitString = static_cast<char_t*>(NULL);
    cfgFile = static_cast<ConfigFile*>(NULL);
  }

  /******************************************************************************
  * getConfigItemId
  ******************************************************************************/
  ConfigIdType BaseConfigItem::getConfigItemId(void) const
  {
    return configItemId;
  }

  /******************************************************************************
  * getName
  ******************************************************************************/
  const char_t* BaseConfigItem::getName(void) const
  {
    return nameString;
  }

  /******************************************************************************
  * getName
  ******************************************************************************/
  const char_t* BaseConfigItem::getGlobalName(void) const
  {
    return globalName;
  }

  /******************************************************************************
  * getDescriptiveText
  ******************************************************************************/
  const char_t* BaseConfigItem::getDescriptiveText(void) const
  {
    return descriptiveText;
  }

  /******************************************************************************
  * getUnitString
  ******************************************************************************/
  const char_t* BaseConfigItem::getUnitString(void) const
  {
    return unitString;
  }

  /******************************************************************************
  * setReadFlagConfigID
  ******************************************************************************/
  void BaseConfigItem::setReadFlagConfigID(void)
  {
    paramRead = true;
  }
  
  /******************************************************************************
  * isReadFlagConfigID
  ******************************************************************************/
  bool BaseConfigItem::isReadFlagConfigID(void) const
  {
    return paramRead; 
  }

  /******************************************************************************
  * getConfigFile
  ******************************************************************************/
  ConfigFile* BaseConfigItem::getConfigFile()
  {
    return cfgFile;
  }

  /******************************************************************************
  * getConfigFile
  ******************************************************************************/
  const ConfigFile* BaseConfigItem::getConfigFile() const
  {
    return cfgFile;
  }
  /******************************************************************************
  * getDatatype
  ******************************************************************************/
  BaseConfigItem::ItemDatatype BaseConfigItem::getDatatype() const
  {
    return itemDatatype;
  }

  /******************************************************************************
  * setGlobalName
  ******************************************************************************/
  bool BaseConfigItem::setGlobalName(const char_t* const globalNameToSet)
  {
    bool retValue = false;
    if (strnlen(globalNameToSet, maxConfigNameLength + 1U) <= maxConfigNameLength)
    {
      static_cast<void>(vfw_strlcpy(&globalName[0], globalNameToSet, sizeof(globalName)));
      retValue = true;
    }

    return retValue;
  }

  /******************************************************************************
  * copyTo
  ******************************************************************************/
  void BaseConfigItem::copyTo(ValueBuffer& buffer, const char_t* const string)
  {
    static_cast<void>(vfw_strlcpy(&buffer.buf[0], string, sizeof(buffer.buf)));
    // No need to make sure string is NULL terminated as vfw_strlcpy guarantees it.
  }

#ifndef _DISPATCHER

  /******************************************************************************
  * writeCrossCompare
  ******************************************************************************/
  void BaseConfigItem::writeCrossCompare(VFW_Buffer* const buffer) const
  {
    vfwPutU16(buffer, configItemId);
    vfwPutString(buffer, &nameString[0]);
    vfwPutString(buffer, &globalName[0]);
    writeCrossCompareBool(buffer, paramRead);
    vfwPutU32(buffer, static_cast<uint32_t>(itemDatatype));

    writeCrossCompareSubClass(buffer);
  }

  /******************************************************************************
  * getWriteCrossCompareMaxSize
  ******************************************************************************/
  uint32_t BaseConfigItem::getWriteCrossCompareMaxSize() const
  {
    // These are the sizes written in writeCrossCompare() above
    // 4 extra for each vfwString
    const uint32_t largestSubClassSize = 4U + valBufferSize + 3U; // size for the largest subclass (string)
    return 2U + 4U + sizeof(nameString) + 4U + sizeof(globalName) + 1U + 4U + largestSubClassSize;
  }

#endif

  /******************************************************************************
  * initBaseCrossCompare
  ******************************************************************************/
  void BaseConfigItem::writeCrossCompareBool(VFW_Buffer* const buffer, const bool value) const
  {
    vfwPutU8(buffer, value ? 255U : 0U);
  }
}
