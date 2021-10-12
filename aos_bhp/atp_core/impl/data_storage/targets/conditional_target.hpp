#ifndef ConditionalTarget_hpp
#define ConditionalTarget_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Declaration for ConditionalTarget class to manage all core actions for a primary target.
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
* 2016-09-19    arastogi    Removed id from constructor
* 2016-09-29    bhidaji     removed permitted speed and added it to base target
*                           Changed setSupervised
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
    * Conditional target class (abstract), derived from BaseTarget.
    *
    * The derived class ConditionalTarget shall be used to store brakeability change target objects.
    * The class manages the base class BaseTarget attributes by calls to constructor methods.
    * BrakeabilityTarget implements the fixed memory pool management for dynamic objects using the
    * AOSCommon::FixedSizeMemoryPool class. The memory pool is defined as a static object to allow
    * the same memory pool object to be shared between all instances of BrakeabilityTarget.
    *
    * Note: In target environment the memory pool is not deallocated, the memory pool shall be
    *       used until the computer is restarted. There might be a need to  deallocate  the memory
    *       pool in simulated environments to avoid memory leaks. Should this be necessary a
    *       deallocator can be implemented and called from the adaptation/application level.
    *
    * Note: Class ConditionalTargets cannot be used to create objects, it is defined
    *       as an abstract class. Conditional targets are not used by the core logic
    *       itself, the core only manages the conditional target objects created by
    *       the adaptation layer.
    */
    class ConditionalTarget : public BaseTarget
    {
    public:

      /**
      * Constructor (explicit)
      *
      * @param[in] tSup    Target supervised state
      * @param[in] tSpeed  Target speed at position [cm/s]
      * @param[in] tPos    Interflo150 reference position of target
      * @param[in] tDir    Travel direction to target
      * @param[in] targOdo Odometer position of target
      */
      ConditionalTarget(const bool tSup, const uint32_t tSpeed,
        const TrackAndPos& tPos, const TravelDir tDir, const OdoPosition targOdo);

      /**
      * Destructor
      *
      * Virtual destructor to deallocate assigned memory
      * Note: Method must be overridden by inherited classes to avoid use of standard heap for dynamic memory.
      */
      ~ConditionalTarget();

      /*************************/
      /* Data access functions */
      /*************************/
      /** Get supervised status
      *
      * @return Return true if conditional target is to be supervised
      */
      virtual bool getSupervisedStatus(void) const;

      /** Is the status for the conditional target changed
      *
      * @return Return statusChanged. This is true if the status of supervise changed, and false otherwise
      */
      bool isStatusChanged(void) const;

      /** Set status changed flag to false
      *
      */
      void resetStatusChanged(void);

      /**
      * Get Changed Speed For Speed Targets
      *
      * @return changed speed for speed target
      */
      virtual uint32_t getSpeedChange() const;

      /**
      * Set Supervised
      * Only used by AbstractTargets!!!
      * This function shall only be used by AbstractTargets
      * Other components need to call setConditionalTargetSupervise() from AbstractTargets
      * to set or reset the supervise status
      * This method checks the supervised status before changing it and if it requires a change
      * statusChanged is set to true.
      * @param[in] supervise   flag to be set for Supervision
      * @return Return true if the function changed the status
      */
     virtual bool setSupervised(const bool supervise);

     /**
     * Add related supervise targets
     *
     */
     void addRelatedSuperviseTargets();

    private:

      /**
      * Default constructor is disabled
      */
      ConditionalTarget();

      /** Supervise status of the target changed
      *
      * Status to indicate whether the supervise status of the train changed or not.
      *   - true = the supervise status has changed and targetCalculation needs to revisit this target
      *   - false = the supervise status was not changed
      */
      bool statusChanged;

      /**
      * Target speed [cm/s]
      *
      * Speed limit to enforce if target is supervised.
      */
      uint32_t speed;

      /**
      * target supervised status
      *
      * Boolean to check if the conditional target should be supervised.
      */
      bool supervised;
    };

  }
}

#endif
