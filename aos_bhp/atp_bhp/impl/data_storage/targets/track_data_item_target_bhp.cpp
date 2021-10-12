/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of TrackDataItemTargetBHP
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2019-04-10    skothiya    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "track_data_item_target_bhp.hpp"
#include "abstract_position.hpp"
#include "targets.hpp"
#include "fixed_size_pool_manager.hpp"
#include "radio_message_types_bhp.hpp"
#include "abstract_target_calculation.hpp"
#include "config.hpp"
/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace
{
  /*Initialize the static class member*/
  ATC::AOSMem::FixedSizeMemoryPool memPool = ATC::AOSMem::FixedSizeMemoryPool();
}

namespace ATP
{
  namespace DS
  {
    /******************************************************************************
    * Parameterized Constructor
    ******************************************************************************/
    TrackDataItemTargetBHP::TrackDataItemTargetBHP(const uint8_t tType,
      const TrackAndPos& tPos, const TravelDir tDir, const OdoPosition tOdo, const uint16_t nValue)
      : TrackDataItemTarget(tType, tPos, tDir, tOdo, nValue)
    {

    }

    /******************************************************************************
    * initMemPoolSizeBHP
    ******************************************************************************/
    bool TrackDataItemTargetBHP::initMemPoolSizeBHP(const uint32_t items)
    {
      bool poolCreatedFlag = false;

      if (!memPool.poolCreatedOk())
      {
        if (memPool.createPool(sizeof(TrackDataItemTargetBHP), static_cast<uint16_t>(items)))
        {
          poolCreatedFlag = true;
        }
      }
      return poolCreatedFlag;
    }

    /******************************************************************************
    * Destructor
    ******************************************************************************/
    TrackDataItemTargetBHP::~TrackDataItemTargetBHP()
    {
    }

    /******************************************************************************
    * new operator
    ******************************************************************************/
    void* TrackDataItemTargetBHP::operator new(size_t const sz)
    {
      void* memBlock = static_cast<void*>(NULL);

      if (sz == memPool.poolBlockSize())
      {
        memBlock = memPool.allocateBlock();
      }
      else
      {
        // Error handler
        ATC::AbstractEventHandler::corePtr()->reportEvent(AbstractTargets::corePtr()->invalidTrackDataMemAllocation, __FILE__
          , __LINE__);
      }

      return memBlock;
    }

    /******************************************************************************
    * delete operator
    ******************************************************************************/
    //lint -esym(1714,ATP::DS::TrackDataItemTargetBHP::operator delete) Lint is wrong, this *is* used
    void TrackDataItemTargetBHP::operator delete(void* const ptr)
    {
      memPool.deAllocateBlock(ptr);
    }

    /******************************************************************************
    * isPassedTargetHandled
    ******************************************************************************/
    bool TrackDataItemTargetBHP::isPassedTargetHandled() const
    {
      bool handleTargetBHP = false;
      const bool validPosition = (Pos::PosKnown == Pos::AbstractPosition::corePtr()->getAccuracyState());

      if ((Kernel::TrackDataTypeLevelCrossing == static_cast<Kernel::TrackDataBHPType> (type)) && validPosition)
      {
        const OdoPosition targetPos = getOdometer();
        const uint32_t approachAreaLen = static_cast<uint32_t>(trackDataItemValue) * 100U;//Converting m to cm
        bool levelCrossingPassed = false;

        if (DirForward == AbstractTargets::corePtr()->getSupposedTravelDir())
        {
          levelCrossingPassed = Pos::AbstractPosition::corePtr()->getLeadingPosOdo() 
                                 > static_cast<OdoPosition>(targetPos + static_cast<OdoPosition> (approachAreaLen));
        }
        else
        {
          levelCrossingPassed = Pos::AbstractPosition::corePtr()->getLeadingPosOdo() 
                                < static_cast<OdoPosition>(targetPos - static_cast<OdoPosition> (approachAreaLen));//Converting m to cm
        }

        if (levelCrossingPassed)
        {
          Targets::instance().setApproachingLevelCrossing(false);
          Targets::instance().setApproachingSpeedLevelCrossing(0U);
          handleTargetBHP = true;
        }
        else
        {
          uint16_t speed = 0U;
          if (!Targets::instance().getApproachingLevelCrossing(speed))
          {
            Targets::instance().setApproachingLevelCrossing(true);
            Targets::instance().setApproachingSpeedLevelCrossing(calcApproachSpeed());
          }
        }
      }
      else
      {
        handleTargetBHP = TrackDataItemTarget::isPassedTargetHandled();
      }
      return handleTargetBHP;
    }

    /******************************************************************************
    * calcApproachSpeed
    ******************************************************************************/
    const uint16_t TrackDataItemTargetBHP::calcApproachSpeed() const
    {
     const uint32_t modeDependentCeilingSpeed = Supv::AbstractTargetCalculation::corePtr()->
                                           calcModeDependentCeilingSpeed(DS::AbstractTargets::corePtr()->getCurCeilingSpeed());

      const uint16_t minApproachSpeedLevelCrossing = Config::instance().getMinApproachSpeed();
      const uint16_t currentSpeed = Pos::AbstractOdometry::corePtr()->getSpeed();
      /*The Approach speed should be:
      - minimum of mode dependent ceiling speed and maximum of current speed and
      - configured minimum approach/level crossing speed.
      */
     const uint16_t approachSpeedLevelCrossing = ATC::ATCMath::minimum
                                            (ATC::ATCMath::maximum
                                            (minApproachSpeedLevelCrossing, currentSpeed), static_cast<uint16_t>(modeDependentCeilingSpeed));
      return approachSpeedLevelCrossing;
    }
  }
}

