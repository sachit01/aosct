/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of GradientTarget
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
* 2016-08-26    bhidaji     Added helper attribute to GradientTarget constructor
*                           Changed gradient => trackGradient
* 2016-09-06    akushwah    Implementation after re-design
* 2016-09-21    bhidaji     gradient changed from uint32_t to int32_t
* 2016-09-23    arastogi    Removed id constructor
* 2016-09-29    bhidaji     removed permitted speed and added it to base target
* 2016-10-26    rquensel    Removed lint warnings
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "gradient_target.hpp"
#include "abstract_targets.hpp"
#include "abstract_event_handler.hpp"
#include "abstract_position.hpp"
#include "fixed_size_pool_manager.hpp"

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
    GradientTarget::GradientTarget(const int8_t tGrad,const TrackAndPos& tPos,
      const TravelDir tDir, const OdoPosition targOdo) : BaseTarget(BaseTarget::GradientTarget, tPos, tDir, targOdo)
    {
      trackGradient = tGrad;
    }

    /******************************************************************************
    * Destructor
    ******************************************************************************/
    GradientTarget::~GradientTarget()
    {
    }

    /******************************************************************************
    * new
    ******************************************************************************/
    void* GradientTarget::operator new(size_t const sz)
    {
      void* ret = static_cast<void*>(NULL);
      if(sz == memPool.poolBlockSize())
      {
        ret = memPool.allocateBlock();
      }
      else
      {
        // Error handler
        ATC::AbstractEventHandler::corePtr()->reportEvent(AbstractTargets::corePtr()->invalidGradientMemAllocation, __FILE__
          , __LINE__);
      }

      return ret;
    }

    /******************************************************************************
    * delete
    ******************************************************************************/
    //lint -esym(1714,ATP::DS::GradientTarget::operator delete) Lint is wrong, this *is* used
    void GradientTarget::operator delete(void* const ptr)
    {
        memPool.deAllocateBlock(ptr);
    }

    /******************************************************************************
    * getTrackGradient
    ******************************************************************************/
    int32_t GradientTarget::getTrackGradient()
    {
      return trackGradient;
    }

    /******************************************************************************
    * initMemPoolSize
    ******************************************************************************/
    bool GradientTarget::initMemPoolSize(const uint32_t items)
    {
      bool poolCreatedFlag = false;

      if (!memPool.poolCreatedOk())
      {
        if (memPool.createPool(sizeof(GradientTarget), static_cast<uint16_t>(items)))
        {
          poolCreatedFlag = true;
        }
      }

      return poolCreatedFlag;
    }

    /******************************************************************************
    * availableMemPool
    ******************************************************************************/
    uint32_t GradientTarget::availableMemPool()
    {
      return memPool.getNumberOfFreeBlocks();
    }

    /******************************************************************************
    * getWriteCrossCompareMaxSizeSubClass
    ******************************************************************************/
    uint32_t GradientTarget::getWriteCrossCompareMaxSizeSubClass() const
    {
      // Se below in writeCrossCompareSubClass for the sizes...
      return 4U;
    }

    /******************************************************************************
    * writeCrossCompareSubClass
    ******************************************************************************/
    void GradientTarget::writeCrossCompareSubClass(VFW_Buffer * const buffer) const
    {
      // Write attributes...
      vfwPutI32(buffer, trackGradient);
    }

    /******************************************************************************
    * isTargetPassed
    ******************************************************************************/
    bool GradientTarget::isTargetPassed() const
    {
      bool targetPassed = false;
      OdoPosition trailingOdoPos = Pos::AbstractPosition::corePtr()->getSafeTrailingPosOdo();
      OdoPosition targetPos = getOdometer();
      TravelDir suppDir = AbstractTargets::corePtr()->getSupposedTravelDir();

      //Should only pass targets if the target direction is same as current direction
      if (getDirection() == suppDir)
      {
        if (suppDir == DirForward)
        {
          //Gradient target removed on trailing end of train passed the target position
          if (targetPos <= trailingOdoPos)
          {
            targetPassed = true;
          }
        }
        else // DirReverse; getDirection() always return forward or reverse
        {
          //Gradient target removed on trailing end of train passed the target position
          if (targetPos >= trailingOdoPos)
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
    bool GradientTarget::isPassedTargetHandled() const
    {
      bool handleTarget = true;
      AbstractTargets::corePtr()->setCurTrackGradient(trackGradient);
      return handleTarget;
    }

  }
}
