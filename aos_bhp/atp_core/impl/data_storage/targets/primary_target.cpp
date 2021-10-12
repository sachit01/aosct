/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of PrimaryTarget
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
* 2016-09-19    arastogi    Removed id constructor
* 2016-10-03    arastogi    Added variable to indicate the time ma will timeout.
* 2016-10-26    rquensel    Removed lint warnings
* 2017-06-20    skothiya    Updated to redefine virtual function isTargetPassed and isPassedTargetHandled
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "primary_target.hpp"
#include "abstract_targets.hpp"
#include "abstract_event_handler.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_odometry.hpp"
#include "abstract_position.hpp"
#include "fixed_size_pool_manager.hpp"
#include "abstract_tracks.hpp"
#include "brake_calculations.hpp"
#include <vfw_string.h>
#ifndef __GNUG__
extern "C" int64_t vfwGetReferenceTime(void);
#else
#include <vfw_time.h>
#endif

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/
namespace
{
  /*Initialize the static class member*/
  ATC::AOSMem::FixedSizeMemoryPool memPool = ATC::AOSMem::FixedSizeMemoryPool();
}

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATP
{
  namespace DS
  {
    /******************************************************************************
    * Parameterized Constructor
    ******************************************************************************/
    PrimaryTarget::PrimaryTarget(const uint32_t tMAId, const uint8_t tRT, const uint32_t tMAMargin, const uint8_t tTimeout,
      const TrackAndPos& tPos, const TravelDir tDir, const OdoPosition targOdo, const char_t* const locationName,
      const LocationType locType, const int8_t locGradient) :BaseTarget(BaseTarget::PrimaryTarget, tPos, tDir, targOdo)
    {
      maId = tMAId;
      routeType = tRT;
      maMargin = tMAMargin;
      maTimeout = tTimeout;

      //if location name not empty
      if (locationName != NULL)
      {
        static_cast<void>(vfw_strlcpy(&locName[0], locationName, sizeof(locName)));
      }
      else
      {
        memset(&locName[0], 0, sizeof(locName));
      }
      locationGradient = locGradient;
      locationType = locType;
      //convert minutes to millisecond and add to current time.
      maTimeoutTimeStamp = vfwGetReferenceTime() + (static_cast<int64_t>(maTimeout) * 60 * 1000);
    }

    /******************************************************************************
    * Destructor
    ******************************************************************************/
    PrimaryTarget::~PrimaryTarget()
    {
      memset(&locName[0], 0, sizeof(locName));
      locationType = UndefinedLocationType;
    }

    /******************************************************************************
    * new
    ******************************************************************************/
    void* PrimaryTarget::operator new(size_t const sz)
    {
      void* ret = static_cast<void*>(NULL);
      if (sz == memPool.poolBlockSize())
      {
        ret = memPool.allocateBlock();
      }
      else
      {
        // Error handler
        ATC::AbstractEventHandler::corePtr()->reportEvent(AbstractTargets::corePtr()->invalidPrimaryMemAllocation, __FILE__
          , __LINE__);
      }

      return ret;
    }

    /******************************************************************************
    * delete
    ******************************************************************************/
    //lint -esym(1714,ATP::DS::PrimaryTarget::operator delete) Lint is wrong, this *is* used
    void PrimaryTarget::operator delete(void* const ptr)
    {
      memPool.deAllocateBlock(ptr);
    }

    /******************************************************************************
    * addRelatedSuperviseTargets
    ******************************************************************************/
    void PrimaryTarget::addRelatedSuperviseTargets(const uint32_t currCS)
    {
      Supv::BrakeCalculations& brkCalc = Supv::BrakeCalculations::instance();
      const TravelDir tDir = getDirection();
      const OdoPosition targOdo = getOdometer();

#ifdef _SYS_INT
      int32_t ebOdometer = targOdo;
      int32_t sbOdometer = targOdo;
      int32_t swOdometer = targOdo;
      int32_t fwOdometer = targOdo;
#else
      int32_t ebOdometer = 0;
      int32_t sbOdometer = 0;
      int32_t swOdometer = 0;
      int32_t fwOdometer = 0;

      //if not a location target, calculate the odometer positions based on gradients in MA
      if(locationType == UndefinedLocationType)
      {
        ebOdometer = brkCalc.findSupvTargetOdo(tDir, targOdo, 0U, brkCalc.getEBCurveDelay(), currCS);
        sbOdometer = brkCalc.findSupvTargetOdo(tDir, targOdo, 0U, brkCalc.getSBCurveDelay(), currCS);
        swOdometer = brkCalc.findSupvTargetOdo(tDir, targOdo, 0U, brkCalc.get2ndwarnCurveDelay(), currCS);
        fwOdometer = brkCalc.findSupvTargetOdo(tDir, targOdo, 0U, brkCalc.get1stwarnCurveDelay(), currCS);
      }
      //if location target, the gradient within location is used to calculate the distance to accelerate from 0 to standstill.
      else
      {
        uint32_t accDeaccDist = brkCalc.calcAccDeaccDistance(0U, 0U, brkCalc.getEBCurveDelay(), locationGradient);
        ebOdometer = (tDir == DirForward)?(targOdo - static_cast<int32_t>(accDeaccDist)): (targOdo + static_cast<int32_t>(accDeaccDist));

        accDeaccDist = brkCalc.calcAccDeaccDistance(0U, 0U, brkCalc.getSBCurveDelay(), locationGradient);
        sbOdometer = (tDir == DirForward)?(targOdo - static_cast<int32_t>(accDeaccDist)): (targOdo + static_cast<int32_t>(accDeaccDist));

        accDeaccDist = brkCalc.calcAccDeaccDistance(0U, 0U, brkCalc.get2ndwarnCurveDelay(), locationGradient);
        swOdometer = (tDir == DirForward)?(targOdo - static_cast<int32_t>(accDeaccDist)): (targOdo + static_cast<int32_t>(accDeaccDist));

        accDeaccDist = brkCalc.calcAccDeaccDistance(0U, 0U, brkCalc.get1stwarnCurveDelay(), locationGradient);
        fwOdometer = (tDir == DirForward)?(targOdo - static_cast<int32_t>(accDeaccDist)): (targOdo + static_cast<int32_t>(accDeaccDist));
      }
#endif

      TrackAndPos ebTnP = AbstractTracks::corePtr()->calculateTrackAndPos(ebOdometer);
      TrackAndPos sbTnP = AbstractTracks::corePtr()->calculateTrackAndPos(sbOdometer);
      TrackAndPos swTnP = AbstractTracks::corePtr()->calculateTrackAndPos(swOdometer);
      TrackAndPos fwTnP = AbstractTracks::corePtr()->calculateTrackAndPos(fwOdometer);

      DS::SupervisedTarget ebTarget(SupervisedTarget::EBPrimaryTarget, this, ebTnP, tDir, ebOdometer);
      DS::SupervisedTarget sbTarget(SupervisedTarget::SBPrimaryTarget, this, sbTnP, tDir, sbOdometer);
      DS::SupervisedTarget swTarget(SupervisedTarget::SWPrimaryTarget, this, swTnP, tDir, swOdometer);
      DS::SupervisedTarget fwTarget(SupervisedTarget::FWPrimaryTarget, this, fwTnP, tDir, fwOdometer);

      //set gradient if its a location target.
      if(locationType != UndefinedLocationType)
      {
        ebTarget.setGradient(locationGradient);
        sbTarget.setGradient(locationGradient);
        swTarget.setGradient(locationGradient);
        fwTarget.setGradient(locationGradient);
      }

      AbstractTargets::corePtr()->addTarget(ebTarget);
      AbstractTargets::corePtr()->addTarget(sbTarget);
      AbstractTargets::corePtr()->addTarget(swTarget);
      AbstractTargets::corePtr()->addTarget(fwTarget);
    }

    /******************************************************************************
    * getLocationType
    ******************************************************************************/
    LocationType PrimaryTarget::getLocationType() const
    {
      return locationType;
    }

    /******************************************************************************
    * getLocationName
    ******************************************************************************/
    bool PrimaryTarget::getLocationName(char_t* const locationName) const
    {
      bool retFlag = true;
      static_cast<void>(vfw_strlcpy(locationName, &locName[0], (maxLocationNameLength + 1U)));
      return retFlag;
    }
    /******************************************************************************
    * getRouteType
    ******************************************************************************/
    uint8_t PrimaryTarget::getRouteType() const
    {
      return routeType;
    }

    /******************************************************************************
    * getMAMargin
    ******************************************************************************/
    uint32_t PrimaryTarget::getMAMargin() const
    {
      return maMargin;
    }

    /******************************************************************************
   * getMATimeout
   ******************************************************************************/
    uint8_t PrimaryTarget::getMATimeout() const
    {
      return maTimeout;
    }

    /******************************************************************************
    * getMATimeout
    ******************************************************************************/
    int64_t PrimaryTarget::getMATimeoutTimeStamp() const
    {
      return maTimeoutTimeStamp;
    }

    /******************************************************************************
    * getLocGradValue
    ******************************************************************************/
    int8_t PrimaryTarget::getLocGradValue() const
    {
      return locationGradient;
    }

    /******************************************************************************
    * initMemPoolSize
    ******************************************************************************/
    bool PrimaryTarget::initMemPoolSize(const uint32_t items)
    {
      bool poolCreatedFlag = false;

      if (!memPool.poolCreatedOk())
      {
        if (memPool.createPool(sizeof(PrimaryTarget), static_cast<uint16_t>(items)))
        {
          poolCreatedFlag = true;
        }
      }

      return poolCreatedFlag;
    }

    /******************************************************************************
    * availableMemPool
    ******************************************************************************/
    uint32_t PrimaryTarget::availableMemPool()
    {
      return memPool.getNumberOfFreeBlocks();
    }

    /******************************************************************************
    * getWriteCrossCompareMaxSizeSubClass
    ******************************************************************************/
    uint32_t PrimaryTarget::getWriteCrossCompareMaxSizeSubClass() const
    {
      // Se below in writeCrossCompareSubClass for the sizes...
      return (4U * 3U) + 1U + 8U;
    }

    /******************************************************************************
    * writeCrossCompareSubClass
    ******************************************************************************/
    void PrimaryTarget::writeCrossCompareSubClass(VFW_Buffer * const buffer) const
    {
      // Write attributes...
      vfwPutU32(buffer, maId);
      vfwPutU32(buffer, static_cast<uint32_t>(routeType));
      vfwPutU32(buffer, maMargin);
      vfwPutU8(buffer, maTimeout);
      vfwPutI64(buffer, maTimeoutTimeStamp);
    }

    /******************************************************************************
    * isTargetPassed
    ******************************************************************************/
    bool PrimaryTarget::isTargetPassed() const
    {
      bool targetPassed = false;
      const OdoPosition leadOdoPos = Pos::AbstractPosition::corePtr()->getLeadingPosOdo();
      OdoPosition targetPos = getOdometer();
     
      TravelDir suppDir = AbstractTargets::corePtr()->getSupposedTravelDir();

      //Should only pass targets if the target direction is same as current direction
      if (getDirection() == suppDir)
      {
        if (suppDir == DirForward)
        {
          targetPos -= static_cast<OdoPosition>(getMAMargin());
          if ((targetPos <= leadOdoPos))
          {
            targetPassed = true;
          }
        }
        else // DirReverse; getDirection() always return forward or reverse
        {
          targetPos += static_cast<OdoPosition>(getMAMargin());
          if ((targetPos >= leadOdoPos))
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
    bool PrimaryTarget::isPassedTargetHandled() const
    {
      bool handleTarget = false;
      ATPMode currentMode = Kernel::AbstractModeControl::corePtr()->getCurrentMode();
      const bool isTrainStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();
     
      if ((ATPModeLocation == currentMode) || (ATPModeSplit == currentMode))
      {
        if (RtNormal == static_cast<RouteType>(routeType))
        {
          handleTarget = true;
        }
      }
      else
      {
        if ((LocationStartTargetType == static_cast<LocationTargetType>(routeType)) ||
          (LocationEndTargetType == static_cast<LocationTargetType>(routeType)))
        {
          handleTarget = true;
        }
        else if (isTrainStandStill)
        {
          handleTarget = true;
        }
        else
        {
          // do nothing
        }
      }

      return handleTarget;
    }

  }
}
