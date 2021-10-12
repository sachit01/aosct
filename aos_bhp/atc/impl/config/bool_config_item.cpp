/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of BoolConfigItem
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-06-22    jeneman     Created
* 2016-07-07    jeneman     Removed things related to writing to NVS
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "bool_config_item.hpp"

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
  BoolConfigItem::BoolConfigItem(const ConfigIdType id, const char_t * const name, const char_t * const desc,
    const char_t * const unit, const bool defVal, ConfigFile * const configFile) :
    BaseConfigItem(id, name, desc, unit, configFile, ItemDatatypeBool)
  {
    value = false;
    defaultValue = defVal;
  }

  /******************************************************************************
  * Data access functions
  ******************************************************************************/
  bool BoolConfigItem::getValue(void) const
  {
    return value;
  }

  void BoolConfigItem::setValue(const bool val)
  {
    value = val;
  }

  /******************************************************************************
  * readValueFromBuffer
  ******************************************************************************/
  bool BoolConfigItem::readValueFromBuffer(VFW_Buffer * const buf)
  {
    // VFW uses the bool_t type for booleans, hence we need to get the value
    bool_t bufVal = vfwGetBOOL(buf);
    // and safely convert it to the standard bool
    value = (bufVal != falseVfw);

    return true; // no range for booleans, so this always succeeds
  }

  /******************************************************************************
  * parseStringToValue
  ******************************************************************************/
  bool BoolConfigItem::parseStringToValue(const char_t * const str)
  {
    // if we do not fail, we will return true for success
    bool retval = true;

    // We interpret "true", "on" and "1" as true and "false", "off" and "0" as false
    if (0 == strncmp("true", str, 4U))
    {
      value = true;
    }
    else if (0 == strncmp("on", str, 2U))
    {
      value = true;
    }
    else if (0 == strncmp("1", str, 1U))
    {
      value = true;
    }
    else if (0 == strncmp("false", str, 5U))
    {
      value = false;
    }
    else if (0 == strncmp("off", str, 3U))
    {
      value = false;
    }
    else if (0 == strncmp("0", str, 1U))
    {
      value = false;
    }
    else
    {
      // Parsing failed, return false and leave value untouched.
      retval = false;
    }

    return retval;
  }

  /******************************************************************************
  * getValueStrings
  ******************************************************************************/
  void BoolConfigItem::getValueStrings(ValueBuffer& valbuf, ValueBuffer& maxbuf, ValueBuffer& minbuf) const
  {
    if (value)
    {
      copyTo(valbuf, "1");
    }
    else
    {
      copyTo(valbuf, "0");
    }

    copyTo(maxbuf, "1");  // For bool, max shall be output as "1"
    copyTo(minbuf, "0");  // For bool, min shall be output as "0"
  }

  /******************************************************************************
  * getValueString
  ******************************************************************************/
  void BoolConfigItem::getValueString(ValueBuffer& valbuf) const
  {
    if (value)
    {
      copyTo(valbuf, "1");
    }
    else
    {
      copyTo(valbuf, "0");
    }
  }

#ifdef _SIL
  bool BoolConfigItem::getDefault(void) const
  {
    return defaultValue;
  }
#endif


  /******************************************************************************
  * initCrossCompare
  ******************************************************************************/
  void BoolConfigItem::writeCrossCompareSubClass(VFW_Buffer* const buffer) const
  {
    writeCrossCompareBool(buffer, value);
    writeCrossCompareBool(buffer, defaultValue);
  }
}
