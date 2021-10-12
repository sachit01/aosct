#ifndef GradientTarget_hpp
#define GradientTarget_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Declaration for GradientTarget class to manage all core actions for a primary target.
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
* 2016-08-26    bhidaji     Added helper attribute to GradientTarget constructor
*                           Changed gradient => trackGradient
* 2016-09-06    akushwah    Implementation after re-design
* 2016-09-21    bhidaji     gradient changed from uint32_t to int32_t
* 2016-09-23    arastogi    Removed id constructor
* 2016-09-29    bhidaji     removed permitted speed and added it to base target
*
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
    *  Gradient target class, derived from BaseTarget.
    *
    * The derived class GradientTarget shall be used to store gradient change target objects.  The
    * class manages the base class BaseTarget attributes by calls to constructor methods.
    * PrimaryTarget implements the fixed memory pool management for dynamic objects using the
    * AOSCommon::FixedSizeMemoryPool class. The memory pool is defined as a static object to allow
    * the same memory pool object to be shared between all instances of PrimaryTarget.
    *
    * Note: In target environment the memory pool is not deallocated, the memory pool shall be
    *       used until the computer is restarted. There might be a need to  deallocate  the memory
    *       pool in simulated environments to avoid memory leaks. Should this be necessary a
    *       deallocator can be implemented and called from the adaptation/application level.
    */
    class GradientTarget : public BaseTarget
    {
    public:

      /**
      * Constructor (explicit)
      *
      * @param[in] tGrad   Track's gradient at position [0.1%]
      * @param[in] tPos    Interflo150 reference position of target
      * @param[in] tDir    Travel direction to target
      * @param[in] targOdo Odometer position of target
      */
      GradientTarget(const int8_t tGrad,
        const TrackAndPos& tPos, const TravelDir tDir, const OdoPosition targOdo);

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
      virtual ~GradientTarget();

      /*************************/
      /* Data access functions */
      /*************************/
      /** Get track's gradient
      *
      * @return  track gradient
      */
      virtual int32_t getTrackGradient();

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
      GradientTarget();

      /**
      * Memory allocator (placement) - disabled
      */
      static void* operator new(size_t const sz, void* const ptr);

      /** Track gradient [0.1%]
      *
      * The gradient value of the track, in permill, from the defined position and in defined direction.
      * In helper targets this value is copied from the original non-helper target to all associated
      * helper targets.
      */
      int32_t trackGradient;
    };
  }
}

#endif
