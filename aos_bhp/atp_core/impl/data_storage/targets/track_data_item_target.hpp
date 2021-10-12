#ifndef TrackDataItemTarget_hpp
#define TrackDataItemTarget_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Declaration for TrackDataItemTarget class to manage all core actions for a primary target.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-16    lantback    Created
* 2016-03-23    lantback    Correction after review
* 2016-07-27    akushwah    Initial Implementation
* 2016-09-06    akushwah    Implementation after re-design
* 2016-09-19    arastogi    Removed id constructor
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "base_target.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace DS
  {
    /**
    * The class TrackDataItemTarget defines and implements the brakability target storage
    * class used within AOS.
    *
    */
    class TrackDataItemTarget : public BaseTarget
    {
    public:

      /**
      * Constructor (explicit)
      *
      * @param[in] tType   Target track data type
      * @param[in] tPos    Interflo150 reference position of target
      * @param[in] tDir    Travel direction to target
      * @param[in] tOdo    Odometer position of target
      * @param[in] nValue    Optional parameter
      */
      TrackDataItemTarget(const uint8_t tType,
        const TrackAndPos& tPos, const TravelDir tDir, const OdoPosition tOdo, const uint16_t nValue = 0U);
      

      /**
      * Memory allocator to allocate memory from memory pool.
      * Note: Method must be overridden by inherited classes to avoid use of standard heap for dynamic memory.
      */
      static void* operator new(size_t const sz);

      /**
      * Memory de-allocator to de-allocate memory to memory pool.
      * Note: Method must be overridden by inherited classes to avoid use of standard heap for dynamic memory.
      */
      static void operator delete(void* const ptr);

      /**
      * Virtual destructor to deallocate assigned memory
      * Note: Method must be overridden by inherited classes to avoid use of standard heap for dynamic memory.
      */
      virtual ~TrackDataItemTarget();

      /*************************/
      /* Data access functions */
      /*************************/
      /** Get track data item type
      *
      * @return Return target track data item number
      */
      virtual TrackDataType getTDIType(void) const;
      
      /** Get track data item value
      *
      * @return Return target track data item value
      */
      uint16_t getTDIValue(void) const;

      /**
      * Initialize/allocate memory pool for dynamic memory substitution
      *
      * @param[in] items Number of items to allocate memory for
      * @return true if memory allocation succeed
      */
      static bool initMemPoolSize(const uint32_t items);

      /**
      * Available memory pool
      *
      * @return number of available and empty slots in allocated memory pool
      */
      static uint32_t availableMemPool(void);

      /** To handle the passed target
      *
      * @return true if target can be removed
      */
      virtual bool isPassedTargetHandled() const;

    protected:

      /**
      * The maximum size for the cross compare data
      *
      * @return Returns the maximum data size for this item
      */
      virtual uint32_t getWriteCrossCompareMaxSizeSubClass() const;

      /**
      * Add attributes to cross compare
      */
      virtual void writeCrossCompareSubClass(VFW_Buffer* const buffer) const;

      /**
      * Target track data item type
      */
      uint8_t  type;

      /**
      * Target track data item type value
      */
      uint16_t trackDataItemValue;

    private:

      /**
      * Default constructor is disabled
      */
      TrackDataItemTarget();

      /**
      * Memory allocator (placement) - disabled
      */
      static void* operator new(size_t const sz, void* const ptr);
    };
  }
}

#endif
