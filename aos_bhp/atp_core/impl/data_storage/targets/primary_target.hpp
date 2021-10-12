#ifndef PrimaryTarget_hpp
#define PrimaryTarget_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Declaration for PrimaryTarget class to manage all core actions for a primary target.
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
* 2016-10-03    arastogi    Added variable to indicate the time ma will timeout.
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
    * Primary target class, derived from BaseTarget.
    *
    * The derived class PrimaryTarget shall be used to store primary target objects.  The class
    * manages the base class BaseTarget attributes by calls to constructor methods. PrimaryTarget
    * implements the fixed memory pool management for dynamic objects using the
    * AOSCommon::FixedSizeMemoryPool class. The memory pool is defined as a static object to allow
    * the same memory pool object to be shared between all instances of PrimaryTarget.
    *
    * Note: In target environment the memory pool is not deallocated, the memory pool shall be
    *       used until the computer is restarted. There might be a need to  deallocate  the memory
    *       pool in simulated environments to avoid memory leaks. Should this be necessary a
    *       deallocator can be implemented and called from the adaptation/application level.
    */
    class PrimaryTarget : public BaseTarget
    {
    public:

      /**
      * Constructor (explicit)
      *
      * @param[in] tMAId          MA id of target
      * @param[in] tRT            Route type of target
      * @param[in] tMAMargin      MA Margin of target [cm]
      * @param[in] tTimeout       Timeout time of target [min]
      * @param[in] tPos           Interflo150 reference position of target
      * @param[in] tDir           Travel direction to target
      * @param[in] targOdo        Odometer position of target
      * @param[in] locationName   Name of location Target
      * @param[in] locType        Type of location Target
      * @param[in] locGradient    Location gradient
      */
      PrimaryTarget(const uint32_t tMAId, const uint8_t tRT, const uint32_t tMAMargin, const uint8_t tTimeout,
        const TrackAndPos& tPos, const TravelDir tDir, const OdoPosition targOdo, const char_t* const locationName = static_cast<const char_t*>(NULL),
        const LocationType locType = UndefinedLocationType, const int8_t locGradient = 0);

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
      virtual ~PrimaryTarget();

      /**
      * Add related supervise targets
      *
      * @param[in] currCS  is the ceiling speed at this target
      */
      void addRelatedSuperviseTargets(const uint32_t currCS);

      /*************************/
      /* Data access functions */
      /*************************/

      /**
      * Get route type
      *
      * @return Return target type
      */
      virtual uint8_t getRouteType() const;

      /**
      * Get MA margin
      *
      * @return Return target MA margin [cm]
      */
      uint32_t getMAMargin(void) const;

      /**
      * Get MA timeout
      *
      * @return Return target MA timeout [min]
      */
      virtual uint8_t getMATimeout(void) const;

      /**
      * Get MA timeout time-stamp
      *
      * @return Return target MA timeout time-stamp[ms]
      */
      int64_t getMATimeoutTimeStamp(void) const;

      /**
      * Get Location Name if Location target
      *
      * @param[out] locationName Name of location
      * @return  true if location Name found
      */
      bool getLocationName(char_t* const locationName) const;

      /**
      * Get location type if location target
      *
      * @return Type of location target
      */
      LocationType getLocationType() const;

      /**
      * Initialize/allocate memory pool for dynamic memory substitution
      *
      * @param[in] items Number of items to allocate memory for
      * @return true if memory allocation succeeded
      */
      static bool initMemPoolSize(const uint32_t items);

      /**
      * Available memory pool
      *
      * @return number of available and empty slots in allocated memory pool
      */
      static uint32_t availableMemPool(void);

      /** To check if target passed
      *
      * @return true if target is passed
      */
      virtual bool isTargetPassed() const;

      /** To handle the passed target
      *
      * @return true if target can be removed
      */
      virtual bool isPassedTargetHandled() const;

      /**
      * Get location gradient value
      *
      * @return Return location gradient value
      */
      virtual int8_t getLocGradValue() const;

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

    private:

      /**
      * Default constructor is disabled
      */
      PrimaryTarget();

      /**
      * Memory allocator (placement) - disabled
      */
      static void* operator new(size_t const sz, void* const ptr);

      /**
      * MA id
      *
      * Assigned MA identity, set by MessageHandler component upon creation of target.
      */
      uint32_t maId;

      /**
      * Route type
      *
      * Defined route type, i.e. type of target.
      */
      uint8_t routeType;

      /**
      * MA margin [cm]
      *
      * Defined MA margin for target, i.e. the acceptable distance before the target position
      * where the train is claimed to "at target".
      */
      uint32_t maMargin;

      /**
      * MA timeout [min]
      *
      * Defined time out for the target
      */
      uint8_t maTimeout;

      /**
      * MA timeout time-stamp [ms]
      *
      * Defined time when target position should be reached. (If time exceeded the target may be
      * locally canceled.)
      */
      int64_t maTimeoutTimeStamp;

      /**
      * Location Name in-case of any location target
      *
      */
      char_t locName[maxLocationNameLength + 1U];

      /**
      * Location Type in case of any location target.
      *
      */
      LocationType  locationType;

      /**
      * Location gradient in case of any location target.
      *
      */
      int8_t locationGradient;

    };
  }
}

#endif
