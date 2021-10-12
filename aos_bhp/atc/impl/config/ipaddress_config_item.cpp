/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of IPaddressConfigItem
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-01-30    spandita   Created and updated the member functions
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "ipaddress_config_item.hpp"
#include "abstract_config_base.hpp"
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
  * Parameterized Constructors
  ******************************************************************************/
  IPaddressConfigItem::IPaddressConfigItem(const ConfigIdType id, const char_t * const name, const char_t * const desc, const char_t * const unit,
    const size_t minimum, const size_t maximum, const char_t * const defVal, ConfigFile * const configFile) :
    BaseConfigItem(id, name, desc, unit, configFile, ItemDatatypeIpaddress)
  {
    maxLength = maximum;
    minLength = minimum;
    if (defVal != NULL)
    {
      copyTo(defaultValue, defVal);
    }
    else
    {
      copyTo(defaultValue, "");
    }
    memset(&value.buf[0], 0, sizeof(value.buf));
    memset(&ipAddress[0], 0, ipAddrLength);
  }

  /******************************************************************************
  * Data access functions
  ******************************************************************************/
  const char_t* IPaddressConfigItem::getValue(void) const
  {
    return value.buf;
  }

  /******************************************************************************
  * setValue
  ******************************************************************************/
  void IPaddressConfigItem::setValue(const char_t * const val)
  {
    if (val != NULL)
    {
      copyTo(value, val);
    }
    else
    {
      AbstractConfigBase::basePtr()->getTrace()->write(3U, "Invalid Value to set: for IP address");
    }
  }

  /******************************************************************************
  * readValueFromBuffer
  ******************************************************************************/
  bool IPaddressConfigItem::readValueFromBuffer(VFW_Buffer * const buf)
  {
    ipAddress[0] = vfwGetU8(buf);
    ipAddress[1] = vfwGetU8(buf);
    ipAddress[2] = vfwGetU8(buf);
    ipAddress[3] = vfwGetU8(buf);
    getValueString(value);

    return true;
  }

  /******************************************************************************
  * parseStringToValue
  ******************************************************************************/
  bool IPaddressConfigItem::parseStringToValue(const char_t * const str)
  {
    // if we do not fail, we will return true for success
    bool retval = true;
    //convert from string to integer
    int32_t filled = sscanf(str, "%hhu.%hhu.%hhu.%hhu", &ipAddress[0], &ipAddress[1], &ipAddress[2], &ipAddress[3]);

    if ((4 == filled) && (0U != ipAddress[0]))
    {
      //do nothing
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
  void IPaddressConfigItem::getValueStrings(ValueBuffer& valbuf, ValueBuffer& maxbuf, ValueBuffer& minbuf) const
  {
    copyTo(valbuf, &value.buf[0]);
    const int32_t res1 = snprintf(&maxbuf.buf[0], sizeof(maxbuf.buf), "%u", maxLength);
    const int32_t res2 = snprintf(&minbuf.buf[0], sizeof(minbuf.buf), "%u", minLength);

    if ((res1 < 0) || (static_cast<size_t>(res1) >= sizeof(valbuf.buf)) ||
        (res2 < 0) || (static_cast<size_t>(res2) >= sizeof(minbuf.buf)))
    {
      ATC::aosHalt(__FILE__, __LINE__, "Could not get valuestring");
    }
  }

  /******************************************************************************
   * getValueString
   ******************************************************************************/
  void IPaddressConfigItem::getValueString(ValueBuffer& valbuf) const
  {
    const int32_t res = snprintf(&valbuf.buf[0], sizeof(valbuf.buf), "%u.%u.%u.%u", ipAddress[0], ipAddress[1], ipAddress[2], ipAddress[3]);

    if ((res < 0) || (static_cast<size_t>(res) >= sizeof(valbuf.buf)))
    {
      ATC::aosHalt(__FILE__, __LINE__, "Could not get valuestring");
    }
  }

#ifdef _SIL
  /******************************************************************************
  * getDefault
  ******************************************************************************/
  const char_t* IPaddressConfigItem::getDefault(void) const
  {
    return defaultValue.buf;
  }
#endif

  /******************************************************************************
  * initCrossCompareSubClass
  ******************************************************************************/
  void IPaddressConfigItem::writeCrossCompareSubClass(VFW_Buffer* const buffer) const
  {  
    vfwPutU8(buffer, ipAddress[0]);
    vfwPutU8(buffer, ipAddress[1]);
    vfwPutU8(buffer, ipAddress[2]);
    vfwPutU8(buffer, ipAddress[3]);
    vfwPutU32(buffer, minLength);
    vfwPutU32(buffer, maxLength);
  }
}

//lint +esym(586,snprintf)
