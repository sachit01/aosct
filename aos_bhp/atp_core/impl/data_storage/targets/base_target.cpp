/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of BaseTarget
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-23    lantback    Created
* 2016-07-27    akushwah    Initial Implementation
* 2016-09-06    akushwah    Implementation after re-design
* 2016-09-21    bhidaji     gradient changed from uint32_t to int32_t
* 2016-09-23    arastogi    Removed id from constructor
* 2016-09-29    bhidaji     Added permitted speed
* 2016-10-26    rquensel    Removed lint warnings
* 2017-06-20    skothiya    Updated to handle train states
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "base_target.hpp"
#include "abstract_targets.hpp"
#include "abstract_position.hpp"
#include "abstract_event_handler.hpp"

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/
namespace
{
  /** Unique id generator
  *
  * The next available id value for target. It is called and incremented every time a target is created.
  */
  uint32_t getnextUniqueIdValue()
  {
    static uint32_t nextUniqueIdValue = 0U;

    //increment the next unique id value
    ++nextUniqueIdValue;

    return nextUniqueIdValue;
  }
}

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATP
{
  namespace DS
  {
    /******************************************************************************
    * parameterized constructor
    ******************************************************************************/
    BaseTarget::BaseTarget(const CoreTargetType classTargetType, const TrackAndPos& tPos, const TravelDir tDir, const OdoPosition targOdo)
    {
      targetType = classTargetType;
      refPos = tPos;
      dir = tDir;
      odo = targOdo;

      //Assign the next available unique id value
      id = getnextUniqueIdValue();

      sentToATO = false;

      if ((dir != DirForward) && (dir != DirReverse))
      {
        // We shall not construct a target with invalid direction
        ATC::AbstractEventHandler::corePtr()->reportEvent(AbstractTargets::corePtr()->invalidDirection, __FILE__
          , __LINE__);
      }
    }

    /******************************************************************************
    * Destructor
    ******************************************************************************/
    BaseTarget::~BaseTarget()
    {
    }

    /******************************************************************************
    * getTargetId
    ******************************************************************************/
    uint32_t BaseTarget::getTargetId() const
    {
      return id;
    }

    /******************************************************************************
    * getPosition
    ******************************************************************************/
    const TrackAndPos& BaseTarget::getPosition() const
    {
      return refPos;
    }

    /******************************************************************************
    * getDirection
    ******************************************************************************/
    TravelDir BaseTarget::getDirection() const
    {
      return dir;
    }

    /******************************************************************************
    * getOdometer
    ******************************************************************************/
    OdoPosition BaseTarget::getOdometer() const
    {
      return odo;
    }

    /******************************************************************************
    * getTargetType
    ******************************************************************************/
    BaseTarget::CoreTargetType BaseTarget::getTargetType() const
    {
      return targetType;
    }

    /******************************************************************************
    * getRouteType
    ******************************************************************************/
    uint8_t BaseTarget::getRouteType(void) const
    {
      return static_cast<uint8_t>(RtUndefined);
    }

    /******************************************************************************
    * getMATimeout
    ******************************************************************************/
    uint8_t BaseTarget::getMATimeout() const
    {
      return 0U;
    }

    /******************************************************************************
    * getLocGradValue
    ******************************************************************************/
    int8_t BaseTarget::getLocGradValue() const
    {
      return 0;
    }

    /******************************************************************************
    * isTargetPassed
    ******************************************************************************/
    bool BaseTarget::isTargetPassed() const
    {
      bool targetPassed = false;
      const OdoPosition leadOdoPos = Pos::AbstractPosition::corePtr()->getLeadingPosOdo();
      const OdoPosition targetPos = getOdometer();
      const TravelDir suppDir = AbstractTargets::corePtr()->getSupposedTravelDir();

      //Should only pass targets if the target direction is same as current direction
      if (dir == suppDir)
      {
        //Check if lead end of train passed the position
        if (suppDir == DirForward)
        {
          if (targetPos <= leadOdoPos)
          {
            targetPassed = true;
          }
        }
        else // DirReverse. dir can only be forward or reverse
        {
          if (targetPos >= leadOdoPos)
          {
            targetPassed = true;
          }
        }
      }
      return targetPassed;
    }

    /******************************************************************************
    * isPassedTargetHandled
    ******************************************************************************/
    bool BaseTarget::isPassedTargetHandled() const
    {
      const bool handleTarget = true;
      return handleTarget;
    }

    /******************************************************************************
    * getWriteCrossCompareMaxSize
    ******************************************************************************/
    uint32_t BaseTarget::getWriteCrossCompareMaxSize() const
    {
      // Se below in writeCrossCompare for the sizes...
      return 4U + 4U + 2U + 4U + 4U + 4U + 1U + getWriteCrossCompareMaxSizeSubClass();
    }

    /******************************************************************************
    * writeCrossCompare
    ******************************************************************************/
    void BaseTarget::writeCrossCompare(VFW_Buffer * const buffer) const
    {
      // Write attributes...
      vfwPutU32(buffer, static_cast<uint32_t>(targetType));
      vfwPutU32(buffer, id);
      vfwPutU16(buffer, refPos.track);
      vfwPutU32(buffer, refPos.position);
      vfwPutU32(buffer, static_cast<uint32_t>(dir));
      vfwPutI32(buffer, odo);
      vfwPutU8(buffer, sentToATO ? 255U : 0U);
      // Write the attributes in the sub class...
      writeCrossCompareSubClass(buffer);
    }

    /******************************************************************************
    * getSpeedChangeReason
    ******************************************************************************/
    SpeedChangeReason BaseTarget::getSpeedChangeReason() const
    {
      return ScrUndefined;
    }

    /******************************************************************************
    * getSpeedChange
    ******************************************************************************/
    uint32_t BaseTarget::getSpeedChange() const
    {
      return 0U;
    }

    /******************************************************************************
    * getTDIType
    ******************************************************************************/
    TrackDataType BaseTarget::getTDIType() const
    {
      return TrackDataTypeUndefined;
    }

  }
}
