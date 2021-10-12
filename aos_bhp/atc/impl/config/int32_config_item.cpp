/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION: 
*  Implementation of Int32ConfigItem
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
#include "int32_config_item.hpp"
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
  Int32ConfigItem::Int32ConfigItem(const ConfigIdType id, const char_t * const name, const char_t * const desc, const char_t * const unit,
                                    const int32_t minimum, const int32_t maximum, const int32_t defVal, ConfigFile * const configFile) :
    BaseConfigItem(id, name, desc, unit, configFile, ItemDatatypeI32)
  {
    min = minimum;
    max = maximum;
    defaultValue = defVal;
    value = 0;
  }

  /******************************************************************************
  * Data access functions
  ******************************************************************************/
  int32_t Int32ConfigItem::getValue(void) const
  {
    return value;
  }

  void Int32ConfigItem::setValue(const int32_t val)
  {
    value = val;
  }

  /******************************************************************************
  * readValueFromBuffer
  ******************************************************************************/
  bool Int32ConfigItem::readValueFromBuffer(VFW_Buffer * const buf)
  {
    int32_t tmp = vfwGetI32(buf);

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
  bool Int32ConfigItem::parseStringToValue(const char_t * const str)
  {
    // if we do not fail, we will return true for success
    bool retval = true;
    int32_t target = 0;

    int32_t filled = sscanf(str, "%d", &target);

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
  void Int32ConfigItem::getValueStrings(ValueBuffer& valbuf, ValueBuffer& maxbuf, ValueBuffer& minbuf) const
  {
    const int32_t res1 = snprintf(&valbuf.buf[0], sizeof(valbuf.buf), "%d", value);
    const int32_t res2 = snprintf(&maxbuf.buf[0], sizeof(maxbuf.buf), "%d", max);
    const int32_t res3 = snprintf(&minbuf.buf[0], sizeof(minbuf.buf), "%d", min);

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
  void Int32ConfigItem::getValueString(ValueBuffer& valbuf) const
  {
    const int32_t res = snprintf(&valbuf.buf[0], sizeof(valbuf.buf), "%d", value);

    if ((res < 0) || (static_cast<size_t>(res) >= sizeof(valbuf.buf)))
    {
      ATC::aosHalt(__FILE__, __LINE__, "Could not get valuestring");
    }
  }

#ifdef _SIL
  /******************************************************************************
  * getDefault
  ******************************************************************************/
  int32_t Int32ConfigItem::getDefault(void) const
  {
    return defaultValue;
  }
#endif

  /******************************************************************************
  * initCrossCompare
  ******************************************************************************/
  void Int32ConfigItem::writeCrossCompareSubClass(VFW_Buffer* const buffer) const
  {
    vfwPutI32(buffer, value);
    vfwPutI32(buffer, min);
    vfwPutI32(buffer, max);
    vfwPutI32(buffer, defaultValue);
  }
}

//lint +esym(586,snprintf)
