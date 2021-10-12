#ifndef CcWrapper_hpp
#define CcWrapper_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file defines Cross Compare Wrapper functions used by SPL-library.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-12-06    rquensel    Created
*
*******************************************************************************/

#include "abstract_cross_compare.hpp"


/**
* The design of this class is a part of the interface used by SPL, so it has to look like this
**/
//lint -esym(1711,CrossCompare) Needed as glue towards the SPL library
class CrossCompare
{ //lint !e1960 Needed as glue towards the SPL library
public:

  CrossCompare();


  /**
  * This is the function that writes data the provided Cross Compare buffer
  *
  * @param[in] ownBuffer   Buffer to write all cross compare data to
  */
  virtual void writeDataBinary(VFW_Buffer* const ownBuffer) const = 0;

  /**
  * Returns the maximum number of bytes that could be written by this object
  *
  */
  virtual uint32_t getMaxDataSize() const = 0;

  /**
  * CrossCompare destructor
  */
  virtual ~CrossCompare();

private:

  /**
  * Internal pointer to the linked list of Cross Compare Objects
  */
  CrossCompare* next_;
};

/**
* The design of this class is a part of the interface used by SPL, so it has to look like this
**/
//lint -esym(1960,CrossCompareFolder) Needed as glue towards the SPL library
class CrossCompareFolder
{ //lint !e1960 Needed as glue towards the SPL library
public:

  /**
  * CrossCompareFolder constructor
  */
  explicit CrossCompareFolder();

  /**
  *
  * This function is shall only be called from the init method. It will add the provided object to the list of cross compare objects.
  *
  * @param[in] mv - The cross compare object to append to the list of cross compare items
  *
  */
  void addObject(CrossCompare* const mv);

  /**
  * CrossCompareFolder destructor
  */
  ~CrossCompareFolder();

private:

  /**
  * CrossCompareFolder copy constructor
  * Not implemented, just here to avoid unintentional copy-construction
  */
  CrossCompareFolder(const CrossCompareFolder&);

  /**
  * CrossCompareFolder assignment operator
  * Not implemented, just here to avoid unintentional assignments
  */
  CrossCompareFolder& operator=(const CrossCompareFolder&);
};

/**
* The design of this class is a part of the interface used by SPL, so it has to look like this
**/
//lint -esym(1960,CrossCompareHandler) Needed as glue towards the SPL library
class CrossCompareHandler
{ //lint !e1960 Needed as glue towards the SPL library
public:

  /**
  * CrossCompareHandler singleton instance method
  */
  static CrossCompareHandler& instance();

  /**
  * CrossCompareHandler destructor
  */
  ~CrossCompareHandler();

private:

  /**
  * CrossCompareHandler constructor
  */
  CrossCompareHandler();

  /**
  * CrossCompareHandler copy constructor
  * Not implemented, just here to avoid unintentional copy-construction
  */
  CrossCompareHandler(const CrossCompareHandler& arg);


  /**
  * CrossCompareHandler assignment operator
  * Not implemented, just here to avoid unintentional assignments
  */
  CrossCompareHandler & operator= (const CrossCompareHandler & other);

  /**
  *
  * Internal attribute to store the current cross compare object.
  * Actually not used, needs to be here in order for the interface
  * to SPL to be persistent.
  */
  const CrossCompare* p_currentLowFrequencyCrossCompare_;

  /**
  *
  * Internal attribute to point to the first cross compare object in the list.
  * Actually not used, needs to be here in order for the interface
  * to SPL to be persistent.
  */
  CrossCompareFolder* p_lowFrequencyFolder_;

  /**
  *
  * Actually not used, needs to be here in order for the interface
  * to SPL to be persistent.
  */
  CrossCompareFolder* p_mediumFrequencyFolder_;

  /**
  *
  * Actually not used, needs to be here in order for the interface
  * to SPL to be persistent.
  */
  CrossCompareFolder* p_highFrequencyFolder_;

};

namespace ATP
{
  namespace IO
  {
    /**
    * class CrossCompareSPL  is a part of the interface used by SPL, so it has to look like this
    */
    class CrossCompareSPL : public ATP::Support::CrossCompareObject
    {
    public:
      /**
      * Constructor
      *
      * @param[in] mv  - Cross Compare Object
      */
      CrossCompareSPL(CrossCompare* const mv = static_cast<CrossCompare*>(NULL));

      /**
      * This is the function that writes data the provided Cross Compare buffer
      *
      * @param[in] ownBuffer  - Buffer to write all cross compare data to
      */
      virtual void writeDataBinary(VFW_Buffer* const ownBuffer) const;

      /**
      * Returns the maximum number of bytes that could be written by this object
      *
      */
      virtual uint32_t getMaxDataSize() const;

    private:
      /**
      * Handle to the cross compare object
      *
      */
      CrossCompare* splCC_;
    };
  } // namespace IO
} // namespace ATP

#endif
