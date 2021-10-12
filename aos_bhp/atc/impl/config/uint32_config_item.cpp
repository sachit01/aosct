/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of Uint32ConfigItem
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
#include "uint32_config_item.hpp"
#include "atc_util.hpp"
#include <cstdio>

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

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATC
{
  /******************************************************************************
  * Constructors
  ******************************************************************************/
  Uint32ConfigItem::Uint32ConfigItem(const ConfigIdType id, const char_t * const name, const char_t * const desc, const char_t * const unit,
    const uint32_t minimum, const uint32_t maximum, const uint32_t defVal, ConfigFile * const configFile) :
    BaseConfigItem(id, name, desc, unit, configFile, ItemDatatypeU32)
  {
    min = minimum;
    max = maximum;
    defaultValue = defVal;
    value = 0U;
  }

  /******************************************************************************
  * getValue
  ******************************************************************************/
  uint32_t Uint32ConfigItem::getValue(void) const
  {
    return value;
  }

  /******************************************************************************
  * setValue
  ******************************************************************************/
  bool Uint32ConfigItem::setValue(const uint32_t val)
  {
    bool success = false;
    const bool withinRange = (max >= val) && (min <= val);

    if (withinRange)
    {
      value = val;
      success = true;
    }

    return success;
  }

  /******************************************************************************
  * readValueFromBuffer
  ******************************************************************************/
  bool Uint32ConfigItem::readValueFromBuffer(VFW_Buffer * const buf)
  {
    uint32_t tmp = vfwGetU32(buf);

    const bool withinRange = (max >= tmp) && (min <= tmp);

    if (withinRange)
    {
      value = tmp;
    }

    return withinRange;
  }

  /******************************************************************************
  * parseStringToValue
  ******************************************************************************/
  bool Uint32ConfigItem::parseStringToValue(const char_t * const str)
  {
    // if we do not fail, we will return true for success
    bool retval = true;
    uint32_t target;

    int32_t filled = sscanf(str, "%u", &target);

    if ((1 == filled) && (max >= target) && (min <= target))
    {
      value = target;
    }
    else
    {
      retval = false;
    }

    return retval;
  }

  /******************************************************************************
  * getValueStrings
  ******************************************************************************/
  void Uint32ConfigItem::getValueStrings(ValueBuffer& valbuf, ValueBuffer& maxbuf, ValueBuffer& minbuf) const
  {
    const int32_t res1 = snprintf(&valbuf.buf[0], sizeof(valbuf.buf), "%u", value);
    const int32_t res2 = snprintf(&maxbuf.buf[0], sizeof(maxbuf.buf), "%u", max);
    const int32_t res3 = snprintf(&minbuf.buf[0], sizeof(minbuf.buf), "%u", min);

    if ((res1 < 0) || (static_cast<size_t>(res1) >= sizeof(valbuf.buf)) ||
        (res2 < 0) || (static_cast<size_t>(res2) >= sizeof(maxbuf.buf)) ||
        (res3 < 0) || (static_cast<size_t>(res3) >= sizeof(minbuf.buf)))
    {
      ATC::aosHalt(__FILE__, __LINE__, "Could not get valuestring");
    }
  }

  /******************************************************************************
  * getValueString
  ******************************************************************************/
  void Uint32ConfigItem::getValueString(ValueBuffer& valbuf) const
  {
    const int32_t res = snprintf(&valbuf.buf[0], sizeof(valbuf.buf), "%u", value);

    if ((res < 0) || (static_cast<size_t>(res) >= sizeof(valbuf.buf)))
    {
      ATC::aosHalt(__FILE__, __LINE__, "Could not get valuestring");
    }
  }

#ifdef _SIL
  /******************************************************************************
  * getDefault
  ******************************************************************************/
  uint32_t Uint32ConfigItem::getDefault(void) const
  {
    return defaultValue;
  }
#endif

  /******************************************************************************
  * initCrossCompare
  ******************************************************************************/
  void Uint32ConfigItem::writeCrossCompareSubClass(VFW_Buffer* const buffer) const
  {
    vfwPutU32(buffer, value);
    vfwPutU32(buffer, min);
    vfwPutU32(buffer, max);
    vfwPutU32(buffer, defaultValue);
  }
}

//lint +esym(586,snprintf)
