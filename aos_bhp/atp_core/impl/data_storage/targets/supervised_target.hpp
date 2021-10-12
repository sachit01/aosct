#ifndef SupervisedTarget_hpp
#define SupervisedTarget_hpp
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
    /** Supervised target class, derived from BaseTarget.
    *
    * The derived class SupervisedTarget is used to store targets that are not received from MA
    * but are created in AOS to help supervising the MA Targets
    * The class manages the base class BaseTarget attributes by calls to constructor methods.
    * SupervisedTarget implements the fixed memory pool management for dynamic objects using the
    * AOSCommon::FixedSizeMemoryPool class. The memory pool is defined as a static object to allow
    * the same memory pool object to be shared between all instances of SupervisedTarget.
    *
    * Note: In target environment the memory pool is not deallocated, the memory pool shall be
    *       used until the computer is restarted. There might be a need to  deallocate  the memory
    *       pool in simulated environments to avoid memory leaks. Should this be necessary a
    *       deallocator can be implemented and called from the adaptation/application level.
    *
    */
    class SupervisedTarget : public BaseTarget
    {
    public:

      /**
      * Definition of Supervised target types
      */
      enum SupervisedTargetType
      {
        Undefined,
        GradientSupvTarget,
        EBSpeedTarget,
        SBSpeedTarget,
        SWSpeedTarget,
        FWSpeedTarget,
        EBPrimaryTarget,
        SBPrimaryTarget,
        SWPrimaryTarget,
        FWPrimaryTarget,
        SpeedIncreaseTarget,
        SMSpeedRestrictionTarget
      };

      /**
      * Constructor (explicit)
      *
      * @param[in] sType   Supervised target type
      * @param[in] ptarget Parent target
      * @param[in] tPos    Interflo150 reference position of target
      * @param[in] tDir    Travel direction to target
      * @param[in] targOdo Odometer position of target
      */
      SupervisedTarget(const SupervisedTargetType sType, BaseTarget* const ptarget,
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
      virtual ~SupervisedTarget();

      /*************************/
      /* Data access functions */
      /*************************/

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

      /** To get the supervised target type
      *
      * @return type supervised target type
      */
      SupervisedTargetType getSupervisedTargetType() const;

      /** Get ceiling speed
      *
      * @return ceiling speed [cm/s]
      */
      uint32_t getCeilingSpeed(void) const;

      /** Get fw ceiling speed
      *
      * @return ceiling speed [cm/s]
      */
      uint32_t getFwCeilingSpeed(void) const;

      /** Get sw ceiling speed
      *
      * @return ceiling speed [cm/s]
      */
      uint32_t getSwCeilingSpeed(void) const;

      /** Get sb ceiling speed
      *
      * @return ceiling speed [cm/s]
      */
      uint32_t getSbCeilingSpeed(void) const;

      /** Get sb ceiling speed
      *
      * @return ceiling speed [cm/s]
      */
      uint32_t getEbCeilingSpeed(void) const;

      /** Get gradient
      *
      * @return gradient [0.1%]
      */
      int8_t getGradient(void) const;

      /** Set ceiling speed
      *
      * @param[in] currentCeilingSpeed  ceilingSpeed value
      */
      void setCeilingSpeed(const uint32_t currentCeilingSpeed);

      /** Set gradient
      *
      * @param[in] currentGradient  Gradient value
      */
      void setGradient(const int32_t currentGradient);

      /** Get first warning speed
      *
      * @return Return first warning speed
      */
      uint32_t getFirstWarningSpeed(void) const;

      /** Set first warning speed
      *
      * @param[in] firstWarnSpeed  first warning speed of the train at the target's position
      */
      void setFirstWarningSpeed(const uint32_t firstWarnSpeed);

      /** Get second warning speed
      *
      * @return Return second warning speed
      */
      uint32_t getSecondWarningSpeed(void) const;

      /** Set second warning speed
      *
      * @param[in] secondWarnSpeed  second warning speed of the train at the target's position
      */
      void setSecondWarningSpeed(const uint32_t secondWarnSpeed);

      /** Get service brake speed
      *
      * @return Return service brake speed
      */
      uint32_t getSBSpeed(void) const;

      /** Set service brake speed
      *
      * @param[in] serviceBrakeSpeed  service brake speed of the train at the target's position
      */
      void setSBSpeed(const uint32_t serviceBrakeSpeed);

      /** Get emergency brake speed
      *
      * @return Return emergency brake speed
      */
      uint32_t getEBSpeed(void) const;

      /** Set emergency brake speed
      *
      * @param[in] emergencyBrakeSpeed  emergency brake speed of the train at the target's position
      */
      void setEBSpeed(const uint32_t emergencyBrakeSpeed);

      /** Set fw ceiling speed
      *
      * @param[in] fwCS ceilingSpeed value
      */
      void setFwCeilingSpeed(const uint32_t fwCS);

      /** Set sw ceiling speed
      *
      * @param[in] swCS ceilingSpeed value
      */
      void setSwCeilingSpeed(const uint32_t swCS);

      /** Set sb ceiling speed
      *
      * @param[in] sbCS ceilingSpeed value
      */
      void setSbCeilingSpeed(const uint32_t sbCS);

      /** Set eb ceiling speed
      *
      * @param[in] ebCS  ceilingSpeed value
      */
      void setEbCeilingSpeed(const uint32_t ebCS);

      /** getParentTarget
      *
      * @return Pointer to the parent target.
      */
      BaseTarget* getParentTarget();

      /** Get if the gradient value is set for this target
      *
      * @return true if the value is set, false otherwise.
      */
      bool isGradientValid(void) const;

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
      SupervisedTarget();

      /**
      * Memory allocator (placement) - disabled
      */
      static void* operator new(size_t const sz, void* const ptr);

      /** Parent Target
      *
      * The target from which the supervised target is created.
      */
      BaseTarget* parentTarget;

      /** Supervised target type
      *
      * Indicates the supervised target type.
      */
      SupervisedTargetType type;

      /** Ceiling speed [cm/s]
      *
      * The ceiling speed that affects the train from this target to the next target.
      */
      uint32_t ceilingSpeed;

      /** Gradient [0.1%]
      *
      * The gradient value that affects the train from this target to the next target.
      */
      int32_t gradient;

      /** isValidGradient [bool]
      *
      * To indicate if the gradient value has been set for this supervised target.
      */
      bool isValidGradient;

      /** Current FW ceiling speed [cm/s]
      *
      * The ceiling speed that affects the train from current position to the next target.
      */
      uint32_t fwCeilingSpeed;

      /** Current SW ceiling speed [cm/s]
      *
      * The ceiling speed that affects the train from current position to the next target.
      */
      uint32_t swCeilingSpeed;

      /** Current SB ceiling speed [cm/s]
      *
      * The ceiling speed that affects the train from current position to the next target.
      */
      uint32_t sbCeilingSpeed;

      /** Current EB ceiling speed [cm/s]
      *
      * The ceiling speed that affects the train from current position to the next target.
      */
      uint32_t ebCeilingSpeed;

      /** First Warning speed [cm/s]
      *
      * The first warning speed is the speed that the train is allowed to have at the position of this target.
      * This means that if the train violates this speed at the position of this target it will not
      * be able to reach the speed restriction imposed from the next target. And also the current target's speed restrictions.
      */
      uint32_t firstWarningSpeed;

      /** Second Warning speed [cm/s]
      *
      * The second warning speed is the second warning curve speed at this target.
      * This means that if the train violates this speed at the position of this target it will issue second warning.
      */
      uint32_t secondWarningSpeed;

      /** Service Brake speed [cm/s]
      *
      * The service brake speed is the service brake curve speed at this target.
      * This means that if the train violates this speed at the position of this target it will issue a service brake.
      */
      uint32_t sbSpeed;

      /** Emergency Brake speed [cm/s]
      *
      * The emergency brake speed is the emergency brake curve speed at this target.
      * This means that if the train violates this speed at the position of this target it will issue a emergency brake.
      */
      uint32_t ebSpeed;

     };
  }
}

#endif
