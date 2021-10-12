/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of Uint16ConfigItem
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
#include "uint16_config_item.hpp"
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
  Uint16ConfigItem::Uint16ConfigItem(const ConfigIdType id, const char_t * const name, const char_t * const desc, const char_t * const unit,
    const uint16_t minimum, const uint16_t maximum, const uint16_t defVal, ConfigFile * const configFile) :
    BaseConfigItem(id, name, desc, unit, configFile, ItemDatatypeU16),
    value(0U),
    min(minimum),
    max(maximum),
    defaultValue(defVal)
  {
  }

  /******************************************************************************
 * Data access functions
 ******************************************************************************/
  uint16_t Uint16ConfigItem::getValue(void) const
  {
    return value;
  }

  void Uint16ConfigItem::setValue(const uint16_t val)
  {
    value = val;
  }

  /******************************************************************************
  * readValueFromBuffer
  ******************************************************************************/
  bool Uint16ConfigItem::readValueFromBuffer(VFW_Buffer * const buf)
  {
    uint16_t tmp = vfwGetU16(buf);

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
  bool Uint16ConfigItem::parseStringToValue(const char_t * const str)
  {
    // if we do not fail, we will return true for success
    bool retval = true;
    uint16_t target;

    int32_t filled = sscanf(str, "%hu", &target);

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
  void Uint16ConfigItem::getValueStrings(ValueBuffer& valbuf, ValueBuffer& maxbuf, ValueBuffer& minbuf) const
  {
    const int32_t res1 = snprintf(&valbuf.buf[0], sizeof(valbuf.buf), "%hu", value);
    const int32_t res2 = snprintf(&maxbuf.buf[0], sizeof(maxbuf.buf), "%hu", max);
    const int32_t res3 = snprintf(&minbuf.buf[0], sizeof(minbuf.buf), "%hu", min);

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
  void Uint16ConfigItem::getValueString(ValueBuffer& valbuf) const
  {
    const int32_t res = snprintf(&valbuf.buf[0], sizeof(valbuf.buf), "%hu", value);

    if ((res < 0) || (static_cast<size_t>(res) >= sizeof(valbuf.buf)))
    {
      ATC::aosHalt(__FILE__, __LINE__, "Could not get valuestring");
    }
  }

#ifdef _SIL
  /******************************************************************************
  * getDefault
  ******************************************************************************/
  uint16_t Uint16ConfigItem::getDefault(void) const
  {
    return defaultValue;
  }
#endif

  /******************************************************************************
  * initCrossCompare
  ******************************************************************************/
  void Uint16ConfigItem::writeCrossCompareSubClass(VFW_Buffer* const buffer) const
  {
    vfwPutU16(buffer, value);
    vfwPutU16(buffer, min);
    vfwPutU16(buffer, max);
    vfwPutU16(buffer, defaultValue);
  }
}

//lint +esym(586,snprintf)
