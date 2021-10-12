/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of StringConfigItem
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
#include "abstract_config_base.hpp"
#include "string_config_item.hpp"
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
  StringConfigItem::StringConfigItem(const ConfigIdType id, const char_t * const name, const char_t * const desc, const char_t * const unit,
    const size_t minimum, const size_t maximum, const char_t * const defVal, ConfigFile * const configFile) :
    BaseConfigItem(id, name, desc, unit, configFile, ItemDatatypeText)
  {
    maxLength = maximum; // Max allowed length of the string
    minLength = minimum; // Min allowed length of the string

    if ((maxLength + 1U) > sizeof(value.buf))
    {
      ATC::aosHalt(__FILE__, __LINE__, "StringConfigItem: Illegal maximum value");
    }
    if (minLength > maxLength)
    {
      ATC::aosHalt(__FILE__, __LINE__, "StringConfigItem: Illegal minimum value");
    }

    if (defVal != NULL)
    {
      copyTo(defaultValue, defVal);
      copyTo(value, defVal);
    }
    else
    {
      copyTo(defaultValue, "");
      copyTo(value, "");
    }

    //Writing the below trace for removing warning and needs to be removed later. 
    AbstractConfigBase::basePtr()->getTrace()->write(9U, "String Config Item Min value :", static_cast<uint32_t>(minimum));
  }

  /******************************************************************************
  * Data access functions
  ******************************************************************************/
  const char_t * StringConfigItem::getValue(void) const
  {
    return value.buf;
  }

  void StringConfigItem::setValue(const char_t * const val)
  {
    if (val != NULL)
    {
      copyTo(value, val);
    }
    else
    {
      copyTo(value, "");
    }
  }

  /******************************************************************************
  * readValueFromBuffer
  ******************************************************************************/
  bool StringConfigItem::readValueFromBuffer(VFW_Buffer * const buf)
  {
    value.buf[0] = '\0'; // in case vfwGetString() fails

    static_cast<void>(vfwGetString(buf, &value.buf[0], maxLength + 1U));

    return true; // TODO implement range check
  }

  /******************************************************************************
  * parseStringToValue
  ******************************************************************************/
  bool StringConfigItem::parseStringToValue(const char_t * const str)
  {
    // if we do not fail, we will return true for success
    bool retval = true;

    size_t len = strnlen(str, maxLength + 1U);

    if ((len >= minLength) && (len <= maxLength))
    {
      copyTo(value, str);
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
  void StringConfigItem::getValueStrings(ValueBuffer& valbuf, ValueBuffer& maxbuf, ValueBuffer& minbuf) const
  {
    copyTo(valbuf, &value.buf[0]);
    // For strings, min shall be output as "0"
    copyTo(minbuf, "0");
    // For strings, max is max number of chars including terminating null.
    const int32_t res = snprintf(&maxbuf.buf[0], sizeof(maxbuf.buf), "%d", maxLength + 1U);

    if ((res < 0) || (static_cast<size_t>(res) >= sizeof(maxbuf.buf)))
    {
      ATC::aosHalt(__FILE__, __LINE__, "Could not get valuestring");
    }
  }

  /******************************************************************************
  * getValueString
  ******************************************************************************/
  void StringConfigItem::getValueString(ValueBuffer& valbuf) const
  {
    copyTo(valbuf, &value.buf[0]);
  }

#ifdef _SIL
  /******************************************************************************
  * getDefault
  ******************************************************************************/
  const char_t* StringConfigItem::getDefault(void) const
  {
    return defaultValue.buf;
  }
#endif

  /******************************************************************************
  * initCrossCompareSubClass
  ******************************************************************************/
  void StringConfigItem::writeCrossCompareSubClass(VFW_Buffer* const buffer) const
  {
    vfwPutString(buffer, &value.buf[0]);
    vfwPutU32(buffer, minLength);
    vfwPutU32(buffer, maxLength);
  }
}

//lint +esym(586,snprintf)
