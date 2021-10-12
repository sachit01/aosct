#ifndef BoolConfigItem_hpp
#define BoolConfigItem_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION: 
*  Declaration for BoolConfigItem class.
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
  /**  bool config item class, derived from BaseConfigItem.
  *
  * The derived class BoolConfigItem shall be used to store config items with a value of type bool.
  * It implements the abstract methods from BaseConfigItem to get and write an bool value.
  *
  */
  class BoolConfigItem : public BaseConfigItem
  {
  public:

    /**
    * Constructor (explicit)
    *
    * @param[in] id          The ID of this item
    * @param[in] name        The name of the item
    * @param[in] desc        A text describing the item
    * @param[in] unit        A text telling which unit the value of this item has (cm, kg, etc)
    * @param[in] defVal      The default value of this item (used for test)
    * @param[in] configFile  Pointer to a ConfigFile (Common, Maintenance etc)
    *  
    */
    BoolConfigItem(const ConfigIdType id, const char_t * const name, const char_t * const desc,
      const char_t * const unit, const bool defVal, ConfigFile * const configFile);


    /*************************/
    /* Data access functions */
    /*************************/
    /**
    * Get value
    *
    * @return the value of the item
    */
    bool getValue(void) const;

    /**
    * Set value
    *
    * @param[in] val  The value to be set as the new item value
    */
    void setValue(const bool val);

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
    * @return true if the string can be parsed and the value fits within the boundaries, false otherwise.
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
    * Implementation of the abstract method to convert the value to text for this item
    *
    * @param[out] valbuf     Pointer to where to store the textual representation of the value
    */
    virtual void getValueString(ValueBuffer& valbuf) const;

#ifdef _SIL
    /**
    * Get default value. Only to be used in SIL environment!
    *
    * @return the default value of the item
    */
    bool getDefault(void) const;
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
    BoolConfigItem();

    /** Value
    *
    * The bool value of the item
    */
    bool value;

    /** Default Value
    *
    * The bool default value of the item
    */
    bool defaultValue;
  };
}

#endif
