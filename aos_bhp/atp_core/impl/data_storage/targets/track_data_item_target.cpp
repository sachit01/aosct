/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of TrackDataItemTarget
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
* 2016-10-26    rquensel    Removed lint warnings
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "track_data_item_target.hpp"
#include "abstract_event_handler.hpp"
#include "abstract_targets.hpp"
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
    * Parameterized Constructor
    ******************************************************************************/
    TrackDataItemTarget::TrackDataItemTarget(const uint8_t tType,
      const TrackAndPos& tPos, const TravelDir tDir, const OdoPosition tOdo, const uint16_t nValue)
      : BaseTarget(BaseTarget::TrackDataItemTarget, tPos, tDir, tOdo)
    {
      type = tType;
      trackDataItemValue = nValue;
    }
    /******************************************************************************
    * Destructor
    ******************************************************************************/
    TrackDataItemTarget::~TrackDataItemTarget()
    {
    }

    /******************************************************************************
    * new
    ******************************************************************************/
    //lint -esym(1714,ATP::DS::TrackDataItemTarget::operator new) Lint is wrong, this *is* used
    void* TrackDataItemTarget::operator new(size_t const sz)
    {
      void* ret = static_cast<void*>(NULL);
      if ((sz == sizeof(TrackDataItemTarget)) && (sz == memPool.poolBlockSize()))
      {
        ret = memPool.allocateBlock();
      }
      else
      {
        // Error handler
        ATC::AbstractEventHandler::corePtr()->reportEvent(AbstractTargets::corePtr()->invalidTrackDataMemAllocation, __FILE__
          , __LINE__);
      }
      return ret;
    }

    /******************************************************************************
    * delete
    ******************************************************************************/
    //lint -esym(1714,ATP::DS::TrackDataItemTarget::operator delete) Lint is wrong, this *is* used
    //lint -esym(1531,ATP::DS::TrackDataItemTarget::operator delete) Lint is wrong, there is no size argument here
    void TrackDataItemTarget::operator delete(void* const ptr)
    {
      memPool.deAllocateBlock(ptr);
    }

    /******************************************************************************
    * getTDIType
    ******************************************************************************/
    TrackDataType TrackDataItemTarget::getTDIType() const
    {
      return (static_cast<TrackDataType>(type));
    }

    /******************************************************************************
    * getTDIType
    ******************************************************************************/
    uint16_t TrackDataItemTarget::getTDIValue() const
    {
      return trackDataItemValue;
    }

    /******************************************************************************
    * initMemPoolSize
    ******************************************************************************/
    bool TrackDataItemTarget::initMemPoolSize(const uint32_t items)
    {
      bool poolCreatedFlag = false;

      if (!memPool.poolCreatedOk())
      {
        if (memPool.createPool(sizeof(TrackDataItemTarget), static_cast<uint16_t>(items)))
        {
          poolCreatedFlag = true;
        }
      }

      return poolCreatedFlag;
    }

    /******************************************************************************
    * availableMemPool
    ******************************************************************************/
    uint32_t TrackDataItemTarget::availableMemPool()
    {
      return memPool.getNumberOfFreeBlocks();
    }

    /******************************************************************************
    * getWriteCrossCompareMaxSizeSubClass
    ******************************************************************************/
    uint32_t TrackDataItemTarget::getWriteCrossCompareMaxSizeSubClass() const
    {
      // Se below in writeCrossCompareSubClass for the sizes...
      return 4U + 2U;
    }

    /******************************************************************************
    * writeCrossCompareSubClass
    ******************************************************************************/
    void TrackDataItemTarget::writeCrossCompareSubClass(VFW_Buffer * const buffer) const
    {
      // Write attributes...
      vfwPutU32(buffer, static_cast<uint32_t>(type));
      vfwPutU16(buffer, trackDataItemValue);
    }

    /******************************************************************************
    * isPassedTargetHandled
    ******************************************************************************/
    bool TrackDataItemTarget::isPassedTargetHandled() const
    {
      bool handleTarget = true;
      switch (type)
      {
      case TrackDataTypePowerSectionLimit:
      case TrackDataTypeWheelLubricationStart:
      case TrackDataTypeWheelLubricationEnd:
        break;
      case TrackDataTypeOdometerInvalidStart:
        AbstractTargets::corePtr()->setOdometerInvalidTargetActive(true);
        break;
      case TrackDataTypeOdometerInvalidEnd:
        AbstractTargets::corePtr()->setOdometerInvalidTargetActive(false);
        break;
      case TrackDataTypeFreeRollingStart:
        AbstractTargets::corePtr()->setFreeRollingTargetActive(true);
        break;
      case TrackDataTypeFreeRollingEnd:
        AbstractTargets::corePtr()->setFreeRollingTargetActive(false);
        break;
      case TrackDataTypeSafetyMarginChange:
        AbstractTargets::corePtr()->setSafetyMarginChangeTargetActive(trackDataItemValue);
        break;
      case TrackDataTypeAdhesionChange:
      case TrackDataTypeAcousticSignal:
      case TrackDataTypeUndefined:
      default:
        break;
      }

      return handleTarget;
    }
  }
}
