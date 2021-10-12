#ifndef StringConfigItem_hpp
#define StringConfigItem_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Declaration for StringConfigItem class.
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
#include "base_config_item.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATC
{
  /** string config item class, derived from BaseConfigItem.
  *
  * The derived class StringConfigItem shall be used to store config items with a string value.
  * It implements the abstract methods from BaseConfigItem to get and write a string value.
  *
  */
  class StringConfigItem : public BaseConfigItem
  {
  public:

    /**
    * Constructor (explicit)
    *
    * This will allocate memory for the string, which should be ok since this is only done at init().
    *
    * @param[in] id          The ID of this item
    * @param[in] name        The name of the item
    * @param[in] desc        A text describing the item (the name of the item)
    * @param[in] unit        A text telling which unit the value of this item has (cm, kg, etc)
    * @param[in] minimum     min size of allowed text for this item
    * @param[in] maximum     longest allowed text for this item
    * @param[in] defVal      The default value of this item (used for test)
    * @param[in] configFile  Pointer to a ConfigFile (Common, Maintenance etc)
    *
    */
    StringConfigItem(const ConfigIdType id, const char_t * const name, const char_t * const desc, const char_t * const unit,
      const size_t minimum, const size_t maximum, const char_t * const defVal, ConfigFile * const configFile);

    /*************************/
    /* Data access functions */
    /*************************/

    /**
    * Get value
    *
    * @return Returns the value of the item
    */
    const char_t* getValue(void) const;

    /**
    * Set value
    *
    * @param[in] val  The value to be set as the new item value
    */
    void setValue(const char_t * const val);

    /**
    * Get the value from the VFW_Buffer and store as value in the item itself
    *
    * Implements the abstract method of BaseConfigItem fetching
    * the correctly typed value for this item from the buffer
    *
    * Prerequisite for this to fetch the correct value is that the buffer pointer
    * is at the correct position in the buffer to get the value for this item
    *
    * @param[in] buf      The buffer from which to get the value
    *
    * @return true if the value can be parsed and the value fits within the boundaries, false otherwise.
    */
    virtual bool readValueFromBuffer(VFW_Buffer * const buf);

    /**
    * Parse supplied string and use it as new value for this item
    *
    * Implements the abstract method to parse the string to the correct type of value for this item
    *
    * @param[in] str      The string to be parsed
    *
    * @return Returns true if the string can be parsed and the value fits within the boundaries, false otherwise.
    */
    virtual bool parseStringToValue(const char_t * const str);

    /**
    * Get the value and boundaries from the item as strings
    *
    * Implementation of the abstract method to convert the value and boundaries to text for this item.
    *
    * @param[out] valbuf     Pointer to where to store the textual representation of the value
    * @param[out] maxbuf     Pointer to where to store the textual representation of the max value
    * @param[out] minbuf     Pointer to where to store the textual representation of the min value
    */
    virtual void getValueStrings(ValueBuffer& valbuf, ValueBuffer& maxbuf, ValueBuffer& minbuf) const;

    /**
    * Get the value from the item as string
    *
    * Implementation of the abstract method to convert the value to text for this item.
    *
    * @param[out] valbuf     Pointer to where to store the textual representation of the value
    */
    virtual void getValueString(ValueBuffer& valbuf) const;

#ifdef _SIL
    /**
    * Get default value. Only to be used in SIL environment!
    *
    * @return Returns the default value of the item
    */
    const char_t* getDefault(void) const;
#endif

  protected:

    /**
    * Abstract method to add attributes of each subclass.
    */
    virtual void writeCrossCompareSubClass(VFW_Buffer* const buffer) const;

  private:

    /**
    * Constructor (default)
    *
    */
    StringConfigItem();


    /** Value
    *
    * Buffer to store the text value for this item
    */
    ValueBuffer value;

    /** Default Value
    *
    * Buffer to store the default value of the item
    */
    ValueBuffer defaultValue;

    /**
    * Declare copy-constructor as private in order to prevent illegal use.
    */
    StringConfigItem(const StringConfigItem&);

    /**
    * Declare assignment-operator as private in order to prevent illegal use.
    */
    StringConfigItem& operator = (const StringConfigItem&);

    /**
    * The minimum allowed length for this item (excluding the nul termination character)
    */
    size_t minLength;

    /**
    * The maximum allowed length for this item (excluding the nul termination character)
    */
    size_t maxLength;
  };
}

#endif
