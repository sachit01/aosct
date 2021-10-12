/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION: 
*  Implementation of Uint64ConfigItem
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
#include "uint64_config_item.hpp"
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
  Uint64ConfigItem::Uint64ConfigItem(const ConfigIdType id, const char_t * const name, const char_t * const desc, const char_t * const unit,
                                      const uint64_t minimum, const uint64_t maximum, const uint64_t defVal, ConfigFile * const configFile) :
    BaseConfigItem(id, name, desc, unit, configFile, ItemDatatypeU64)
  {
    min = minimum;
    max = maximum;
    defaultValue = defVal;
    value = 0U;
  }

  /******************************************************************************
  * Data access functions
  ******************************************************************************/
  uint64_t Uint64ConfigItem::getValue(void) const
  {
    return value;
  }

  /******************************************************************************
  * setValue
  ******************************************************************************/
  void Uint64ConfigItem::setValue(const uint64_t val)
  {
    value = val;
  }

  /******************************************************************************
  * readValueFromBuffer
  ******************************************************************************/
  bool Uint64ConfigItem::readValueFromBuffer(VFW_Buffer * const buf)
  {
    uint64_t tmp = vfwGetU64(buf);

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
  bool Uint64ConfigItem::parseStringToValue(const char_t * const str)
  {
    // if we do not fail, we will return true for success
    bool retval = true;
    uint64_t target;

    int32_t filled = sscanf(str, "%llu", &target);

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
  void Uint64ConfigItem::getValueStrings(ValueBuffer& valbuf, ValueBuffer& maxbuf, ValueBuffer& minbuf) const
  {
    const int32_t res1 = snprintf(&valbuf.buf[0], sizeof(valbuf.buf), "%llu", value);
    const int32_t res2 = snprintf(&maxbuf.buf[0], sizeof(maxbuf.buf), "%llu", max);
    const int32_t res3 = snprintf(&minbuf.buf[0], sizeof(minbuf.buf), "%llu", min);

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
  void Uint64ConfigItem::getValueString(ValueBuffer& valbuf) const
  {
    const int32_t res = snprintf(&valbuf.buf[0], sizeof(valbuf.buf), "%llu", value);
    if ((res < 0) || (static_cast<size_t>(res) >= sizeof(valbuf.buf)))
    {
      ATC::aosHalt(__FILE__, __LINE__, "Could not get valuestring");
    }
  }

#ifdef _SIL
  /******************************************************************************
  * getDefault
  ******************************************************************************/
  uint64_t Uint64ConfigItem::getDefault(void) const
  {
    return defaultValue;
  }
#endif

  /******************************************************************************
  * initCrossCompare
  ******************************************************************************/
  void Uint64ConfigItem::writeCrossCompareSubClass(VFW_Buffer* const buffer) const
  {
    vfwPutU64(buffer, value);
    vfwPutU64(buffer, min);
    vfwPutU64(buffer, max);
    vfwPutU64(buffer, defaultValue);
  }
}

//lint +esym(586,snprintf)
