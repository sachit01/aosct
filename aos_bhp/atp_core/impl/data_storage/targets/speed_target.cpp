/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of SpeedTarget
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
* 2016-09-29    bhidaji     removed permitted speed and added it to base target
* 2016-10-26    rquensel    Removed lint warnings
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "speed_target.hpp"
#include "abstract_targets.hpp"
#include "abstract_tracks.hpp"
#include "abstract_event_handler.hpp"
#include "abstract_position.hpp"
#include "fixed_size_pool_manager.hpp"
#include "brake_calculations.hpp"
#include "atc_math.hpp"
#include "abstract_tsetup.hpp"

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
    * Constructor
    ******************************************************************************/
    SpeedTarget::SpeedTarget(const uint32_t tSpd, const uint32_t tReason,
      const TrackAndPos& tPos, const TravelDir tDir, const OdoPosition targOdo)
      :BaseTarget(BaseTarget::SpeedTarget, tPos, tDir, targOdo)
    {
      speed = tSpd;
      reason = static_cast<SpeedChangeReason>(tReason);
    }

    /******************************************************************************
    * Destructor
    ******************************************************************************/
    SpeedTarget::~SpeedTarget()
    {
    }

    /******************************************************************************
    * new
    ******************************************************************************/
    void* SpeedTarget::operator new(size_t const sz)
    {
      void* ret = static_cast<void*>(NULL);
      if (sz == memPool.poolBlockSize())
      {
        ret = memPool.allocateBlock();
      }
      else
      {
        // Error handler
        ATC::AbstractEventHandler::corePtr()->reportEvent(AbstractTargets::corePtr()->invalidSpeedMemAllocation, __FILE__
          , __LINE__);
      }

      return ret;
    }

    /******************************************************************************
    * delete
    ******************************************************************************/
    //lint -esym(1714,ATP::DS::SpeedTarget::operator delete) Lint is wrong, this *is* used
    void SpeedTarget::operator delete(void* const ptr)
    {
      memPool.deAllocateBlock(ptr);
    }

    /******************************************************************************
    * addRelatedSuperviseTargets
    ******************************************************************************/
    void SpeedTarget::addRelatedSuperviseTargets(const uint32_t currCS, const int32_t currGrad)
    {
      const TravelDir tDir = getDirection();
      const OdoPosition targOdo = getOdometer();

      //if this speed target increases the ceiling speed or the speed target has same speed, it will not have 4 targets.
      if (speed >= currCS)
      {
        //Add a supervised target at the ceiling speed target position.
        DS::SupervisedTarget speedIncTarget(DS::SupervisedTarget::SpeedIncreaseTarget,
          this, getPosition(), tDir, targOdo);
        DS::AbstractTargets::corePtr()->addTarget(speedIncTarget);
      }
      else
      {
        uint32_t warningLimit = Supv::BrakeCalculations::instance().calcWarningLimitMargin(speed) + speed;
        uint32_t sbLimit = Supv::BrakeCalculations::instance().calcServiceBrakeLimitMargin(speed) + speed;
        uint32_t ebLimit = Supv::BrakeCalculations::instance().calcEmergencyBrakeLimitMargin(speed) + speed;

        uint32_t ebDelay = (Supv::BrakeCalculations::instance().getEBCurveDelay() + 5U) / 10U;
        uint32_t sbDelay = (Supv::BrakeCalculations::instance().getSBCurveDelay() + 5U) / 10U;
        uint32_t swDelay = (Supv::BrakeCalculations::instance().get2ndwarnCurveDelay() + 5U) / 10U;
        uint32_t fwDelay = (Supv::BrakeCalculations::instance().get1stwarnCurveDelay() + 5U) / 10U;

        int32_t ebOdometer = 0;
        int32_t sbOdometer = 0;
        int32_t swOdometer = 0;
        int32_t fwOdometer = 0;

        int32_t adjGrad = ATC::ATCMath::minimum(currGrad, 0);

        //get the worst deceleration for the speed and adjust it with the gradient.
        int32_t deceleration = DS::AbstractTSetup::corePtr()->getWorstBrakeabilityInRange(currCS, speed);
        deceleration = Supv::BrakeCalculations::instance().calcDeceleration(deceleration, currGrad);

        if (DirForward == tDir)
        {
          ebOdometer = targOdo - calcSupTargOdoOffset(ebLimit, ebDelay, adjGrad, deceleration);
          sbOdometer = targOdo - calcSupTargOdoOffset(sbLimit, sbDelay, adjGrad, deceleration);
          swOdometer = targOdo - calcSupTargOdoOffset(warningLimit, swDelay, adjGrad, deceleration);
          fwOdometer = targOdo - calcSupTargOdoOffset(speed, fwDelay, adjGrad, deceleration);

        }
        else
        {
          ebOdometer = targOdo + calcSupTargOdoOffset(ebLimit, ebDelay, adjGrad, deceleration);
          sbOdometer = targOdo + calcSupTargOdoOffset(sbLimit, sbDelay, adjGrad, deceleration);
          swOdometer = targOdo + calcSupTargOdoOffset(warningLimit, swDelay, adjGrad, deceleration);
          fwOdometer = targOdo + calcSupTargOdoOffset(speed, fwDelay, adjGrad, deceleration);
        }

        DS::SupervisedTarget ebTarget(DS::SupervisedTarget::EBSpeedTarget, this,
          DS::AbstractTracks::corePtr()->calculateTrackAndPos(ebOdometer), tDir, ebOdometer);
        DS::AbstractTargets::corePtr()->addTarget(ebTarget);

        DS::SupervisedTarget sbTarget(DS::SupervisedTarget::SBSpeedTarget, this,
          DS::AbstractTracks::corePtr()->calculateTrackAndPos(sbOdometer), tDir, sbOdometer);
        DS::AbstractTargets::corePtr()->addTarget(sbTarget);

        DS::SupervisedTarget swTarget(DS::SupervisedTarget::SWSpeedTarget, this,
          DS::AbstractTracks::corePtr()->calculateTrackAndPos(swOdometer), tDir, swOdometer);
        DS::AbstractTargets::corePtr()->addTarget(swTarget);

        DS::SupervisedTarget fwTarget(DS::SupervisedTarget::FWSpeedTarget, this,
          DS::AbstractTracks::corePtr()->calculateTrackAndPos(fwOdometer), tDir, fwOdometer);
        DS::AbstractTargets::corePtr()->addTarget(fwTarget);
      }
    }

    /******************************************************************************
    * calcSupTargOdoOffset
    ******************************************************************************/
    int32_t SpeedTarget::calcSupTargOdoOffset(const uint32_t speedLimit, const uint32_t delay, 
      const int32_t gradient, const int32_t deceleration) const
    {
      ATC::ATCMath& math = ATC::ATCMath::instance();
      //v1 is the speed after the delay time. The speed increases due to the gradient during the delay time.
      int32_t v1 = static_cast<int32_t>(speedLimit)
                  - static_cast<int32_t>(math.signMul(gradient, delay, __FILE__, __LINE__));

      //Distance traveled during delay time at current speed limit = speedLimit * delay.
      int32_t delaySpeedDist = static_cast<int32_t>(math.signMul(speedLimit, delay, __FILE__, __LINE__));

      //Distance traveled during delay due to gradient acceleration = (gradient * delay^2)/2.
      //this value is subtracted as it is negative for downhill gradients.
      int32_t delayGradDist = static_cast<int32_t>(math.signMul((delay * delay), gradient, __FILE__, __LINE__) / 2);

      //Distance traveled when the brakes are applied after the delay time and the time the speed reduces to targetSP.
      // this is equal to ((speedLimit - gradient*delay)^2 - speedLimit^2)/(2*deceleration)
      int32_t delayAccDeaccDist = static_cast<int32_t>(math.signDiv(
                  static_cast<int32_t>(math.signMul(v1, v1, __FILE__, __LINE__) - math.signMul(speedLimit, speedLimit, __FILE__, __LINE__)),
                                  2 * deceleration,__FILE__, __LINE__));

      return ((delaySpeedDist + delayAccDeaccDist) - delayGradDist);
    }

    /******************************************************************************
    * getSpeedChange
    ******************************************************************************/
    uint32_t SpeedTarget::getSpeedChange() const
    {
      return speed;
    }

    /******************************************************************************
    * getSpeedChangeReason
    ******************************************************************************/
    SpeedChangeReason SpeedTarget::getSpeedChangeReason() const
    {
      return reason;
    }

    /******************************************************************************
    * initMemPoolSize
    ******************************************************************************/
    bool SpeedTarget::initMemPoolSize(const uint32_t items)
    {
      bool poolCreatedFlag = false;

      if (!memPool.poolCreatedOk())
      {
        if (memPool.createPool(sizeof(SpeedTarget), static_cast<uint16_t>(items)))
        {
          poolCreatedFlag = true;
        }
      }

      return poolCreatedFlag;
    }

    /******************************************************************************
    * availableMemPool
    ******************************************************************************/
    uint32_t SpeedTarget::availableMemPool()
    {
      return memPool.getNumberOfFreeBlocks();
    }

    /******************************************************************************
    * getWriteCrossCompareMaxSizeSubClass
    ******************************************************************************/
    uint32_t SpeedTarget::getWriteCrossCompareMaxSizeSubClass() const
    {
      // Se below in writeCrossCompareSubClass for the sizes...
      return 4U + 4U;
    }

    /******************************************************************************
    * writeCrossCompareSubClass
    ******************************************************************************/
    void SpeedTarget::writeCrossCompareSubClass(VFW_Buffer * const buffer) const
    {
      // Write attributes...
      vfwPutU32(buffer, speed);
      vfwPutU32(buffer, static_cast<uint32_t>(reason));
    }

    /******************************************************************************
    * isPassedTargetHandled
    ******************************************************************************/
    bool SpeedTarget::isPassedTargetHandled() const
    {
      bool handleTarget = true;
      AbstractTargets::corePtr()->setCurCeilingSpeed(speed);
      return handleTarget;
    }
  }
}
