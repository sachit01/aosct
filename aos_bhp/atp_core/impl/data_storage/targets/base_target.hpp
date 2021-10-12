#ifndef BaseTarget_hpp
#define BaseTarget_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Declaration for BaseTarget class, used as foundation for all targets managed by AOS.
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
* 2016-09-21    bhidaji     gradient changed from uint32_t to int32_t
* 2016-09-23    arastogi    Removed id from constructor
* 2016-09-29    bhidaji     Added permitted speed
* 2016-10-27    rquensel    Removed lint warnings
* 2017-06-20    skothiya    Updated to handle train states
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atp_types.hpp"
#include "event.hpp"
#include <vector>

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace DS
  {
    /**
    * Base class (abstract) for all targets
    *
    * The class BaseTarget is an abstract class representing the common parts of a generic
    * target. The class is used to allow a common GP handling of targets such as insertion in
    * travel direction order.
    * The BaseTarget class defines a target type structure to identify the base and derived
    * classes and allow generic processing. The target type is represented by two values:
    *   - type, representing the GP/core class
    *   - subTyp, representing the GA/adaptation type of derived class.
    * In combination these specifies how the target will be treated. Derived classes must
    * implement functions to get the associated types.
    * Note: It is the responsibility of the derived classes to correctly manage the class
    * identities to allow generic (GP) processing.
    *
    * The BaseTarget shall be used in derived classes to create manageable objects.
    */
    class BaseTarget
    {
    public:
      /**
      * Definition of core target types
      */
      enum CoreTargetType
      {
        PrimaryTarget,
        SpeedTarget,
        GradientTarget,
        ConditionalTarget,
        TrackDataItemTarget,
        SupervisedTarget
      };

      /**
      * Definition of Location target types
      */
      enum LocationTargetType
      {
        LocationStartTargetType = 1,  //!< Location Start Target Type
        LocationEndTargetType = 2     //!< Location End Target Type
      };

      /**
      * Constructor (explicit)
      *
      * @param[in] classTargetType    Target type
      * @param[in] tPos               Interflo150 reference position of target
      * @param[in] tDir               Travel direction to target
      * @param[in] targOdo            Odometer position of target
      */
      BaseTarget(const CoreTargetType classTargetType, const TrackAndPos& tPos, const TravelDir tDir, const OdoPosition targOdo);

      /**
      * Destructor
      *
      * Virtual destructor to deallocate assigned memory
      * Note: Method must be overridden by inherited classes to avoid use of standard heap for dynamic memory.
      */
      virtual ~BaseTarget();

      /**
      * Retrieves target base type, i.e. the inherited base class variant which the core
      * functionality shall use when processing the target.
      *
      * Note: This method shall be implemented within the core part, not adaptation.
      *
      * @return Target type constant (core)
      */
      CoreTargetType getTargetType() const;

      /*************************/
      /* Data access functions */
      /*************************/

      /**
      * Get target id
      *
      * @return Returns target id
      */
      uint32_t getTargetId() const;

      /**
      * Get Interflo150 reference position
      *
      * @return Returns target reference position
      */
      const TrackAndPos& getPosition() const;

      /**
      * Get travel direction
      *
      * @return Returns travel direction, i.e. in which direction the target is valid
      */
      TravelDir getDirection() const;

      /**
      * Get odometer position
      *
      * @return Returns odometer value for target
      */
      int32_t getOdometer() const;

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
      * The maximum size for the cross compare data
      *
      * @return Returns the maximum data size for this item
      */
      uint32_t getWriteCrossCompareMaxSize() const;

      /**
      * Add attributes to cross compare
      * @param[in] buffer        The cross compare buffer to get store the attributes in
      */
      void writeCrossCompare(VFW_Buffer* const buffer) const;

      /**
      * Get route type of primary target
      *
      * @return target type
      */
      virtual uint8_t getRouteType() const;

      /**
      * Get Ma timeout of primary target
      *
      * @return Return target MA timeout [min]
      */
      virtual uint8_t getMATimeout() const;

      /**
      * Get location gradient value 
      *
      * @return location gradient value
      */
      virtual int8_t getLocGradValue() const;

      /**
      * Get Speed Change Reason
      *
      * @return speed change reason
      */
      virtual SpeedChangeReason getSpeedChangeReason() const;

      /**
      * Get Changed Speed For Target
      *
      * @return changed speed for target
      */
      virtual uint32_t getSpeedChange() const;

      /*************************/
      /* Data access functions */
      /*************************/
      /** Get track data item type
      *
      * @return Return target track data item number
      */
      virtual TrackDataType getTDIType(void) const;

    protected:

      /**
      * The maximum size for the cross compare data
      *
      * @return Returns the maximum data size for this item
      */
      virtual uint32_t getWriteCrossCompareMaxSizeSubClass() const = 0;

      /**
      * Add attributes to cross compare
      */
      virtual void writeCrossCompareSubClass(VFW_Buffer* const buffer) const = 0;

    private:

      /**
      * Default constructor is disabled
      */
      BaseTarget();

      /**
      * Target type
      *
      * Target type for which the processing is being done.
      */
      CoreTargetType targetType;

      /**
      * Target id
      *
      * Unique identity number of target object.
      */
      uint32_t      id;

      /**
      * Target reference position
      *
      * Defined position of target in the Interflo150 referencing system, i.e. using track
      * number and track position.
      */
      TrackAndPos   refPos;

      /**
      * Travel direction
      *
      * Travel direction for which the target is valid
      */
      TravelDir     dir;

      /**
      * Target odometer position
      *
      * The odometer value representing the position of the target.
      */
      int32_t       odo;

      /**
      * Sent to ATO flag
      *
      * Status flag indicating if target has been forwarded to ATO or not.
      *
      * Note: This status flag is only used by the ATO communication interface but shall be
      *       present in all targets thus made part of BaseTarget class.
      */
      bool          sentToATO;

    };
  }
}

#endif
