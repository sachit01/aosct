#ifndef SpeedTarget_hpp
#define SpeedTarget_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Declaration for SpeedTarget class to manage all core actions for a primary target.
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
* 2016-08-25    bhidaji     Added changes needed for TargetCalculation
* 2016-09-06    akushwah    Implementation after re-design
* 2016-09-19    arastogi    Removed id constructor
* 2016-09-29    bhidaji     removed permitted speed and added it to base target
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
    /** Speed change target class, derived from BaseTarget.
    *
    * The derived class SpeedTarget shall be used to store speed change target objects.
    * The class manages the base class BaseTarget attributes by calls to constructor methods.
    * SpeedTarget implements the fixed memory pool management for dynamic objects using the
    * AOSCommon::FixedSizeMemoryPool class. The memory pool is defined as a static object to allow
    * the same memory pool object to be shared between all instances of SpeedTarget.
    *
    * Note: In target environment the memory pool is not deallocated, the memory pool shall be
    *       used until the computer is restarted. There might be a need to  deallocate  the memory
    *       pool in simulated environments to avoid memory leaks. Should this be necessary a
    *       deallocator can be implemented and called from the adaptation/application level.
    *
    */
    class SpeedTarget : public BaseTarget
    {
    public:

      /**
      * Constructor (explicit)
      *
      * @param[in] tSpd    Target speed at position [cm/s]
      * @param[in] tReason Reason for speed change
      * @param[in] tPos    Interflo150 reference position of target
      * @param[in] tDir    Travel direction to target
      * @param[in] targOdo Odometer position of target
      */
      SpeedTarget(const uint32_t tSpd, const uint32_t tReason,
        const TrackAndPos& tPos, const TravelDir tDir, const OdoPosition targOdo);

      /**
      * Memory allocator to allocate memory from memory pool.
      * Note: Method must be overridden by inherited classes to avoid use of standard heap for dynamic memory.
      */
      static void* operator new(size_t const sz);

      /**
      * Virtual memory de-allocator to de-allocate memory to memory pool.
      * Note: Method must be overridden by inherited classes to avoid use of standard heap for dynamic memory.
      */
      static void operator delete(void* const ptr);

      /**
      * Virtual destructor to deallocate assigned memory
      * Note: Method must be overridden by inherited classes to avoid use of standard heap for dynamic memory.
      */
      virtual ~SpeedTarget();

      /**
      * Add related supervise targets
      *
      * @param[in] currCS  is the last ceiling speed before this target
      * @param[in] currGrad  is the last gradient before this target
      */
      void addRelatedSuperviseTargets(const uint32_t currCS, const int32_t currGrad);

      /*************************/
      /* Data access functions */
      /*************************/
      /**
      * Get target speed change
      *
      * @return Return target speed
      */
      virtual uint32_t getSpeedChange() const;

      /**
      * Get speed change reason
      *
      * @return Return target speed change reason
      */
      virtual SpeedChangeReason getSpeedChangeReason() const;


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

    private:

      /**
      * Default constructor is disabled
      */
      SpeedTarget();

      /**
      * Memory allocator (placement) - disabled
      */
      static void* operator new(size_t const sz, void* const ptr);

      /**
      * Calculate the offsets for supervised targets
      *
      * @param[in] speedLimit     The fw, sw, sb or eb ceiling speed for this target
      * @param[in] delay          The brake curve delay for fw, sw, sb or eb curves
      * @param[in] gradient       The gradient at the speed target.
      * @param[in] deceleration   The deceleration at the speed target.
      * @return the distance to offset the supervise target based on the parameters.
      */
      int32_t calcSupTargOdoOffset(const uint32_t speedLimit,const uint32_t delay,const int32_t gradient,const int32_t deceleration) const;

      /**
      * Target speed [cm/s]
      *
      * Target speed to be enforced
      */
      uint32_t speed;

      /**
      * Speed change reason
      *
      * Code representing the speed change.
      */
      SpeedChangeReason reason;

     };
  }
}

#endif
