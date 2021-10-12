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
#include "supervised_target.hpp"
#include "abstract_targets.hpp"
#include "abstract_event_handler.hpp"
#include "abstract_position.hpp"
#include "fixed_size_pool_manager.hpp"
#include "brake_calculations.hpp"
#include "atc_math.hpp"

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
    * Destructor
    ******************************************************************************/
    SupervisedTarget::~SupervisedTarget()
    {
      parentTarget = static_cast<BaseTarget*>(NULL);
    }

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    SupervisedTarget::SupervisedTarget(const SupervisedTargetType sType,BaseTarget* const ptarget,
      const TrackAndPos& tPos, const TravelDir tDir, const OdoPosition targOdo)
      :BaseTarget(BaseTarget::SupervisedTarget, tPos, tDir, targOdo)
    {
      //set all odometer values to target odometer
      ceilingSpeed = 0U;
      gradient = 0;
      isValidGradient = false;

      fwCeilingSpeed = 0U;
      swCeilingSpeed = 0U;
      sbCeilingSpeed = 0U;
      ebCeilingSpeed = 0U;

      firstWarningSpeed = 0U;
      secondWarningSpeed = 0U;
      sbSpeed = 0U;
      ebSpeed = 0U;
      parentTarget = ptarget;
      type = sType;
    }

    /******************************************************************************
    * new
    ******************************************************************************/
    void* SupervisedTarget::operator new(size_t const sz)
    {
      void* ret = static_cast<void*>(NULL);
      if (sz == memPool.poolBlockSize())
      {
        ret = memPool.allocateBlock();
      }
      else
      {
        // Error handler
        ATC::AbstractEventHandler::corePtr()->reportEvent(AbstractTargets::corePtr()->invalidSupervisedMemAllocation, __FILE__
          , __LINE__);
      }

      return ret;
    }

    /******************************************************************************
    * delete
    ******************************************************************************/
    void SupervisedTarget::operator delete(void* const ptr)
    {
      memPool.deAllocateBlock(ptr);
    }

    /******************************************************************************
    * getSupervisedTargetType
    ******************************************************************************/
    SupervisedTarget::SupervisedTargetType SupervisedTarget::getSupervisedTargetType() const
    {
      return type;
    }

    /******************************************************************************
    * initMemPoolSize
    ******************************************************************************/
    bool SupervisedTarget::initMemPoolSize(const uint32_t items)
    {
      bool poolCreatedFlag = false;

      if (!memPool.poolCreatedOk())
      {
        if (memPool.createPool(sizeof(SupervisedTarget), static_cast<uint16_t>(items)))
        {
          poolCreatedFlag = true;
        }
      }

      return poolCreatedFlag;
    }

    /******************************************************************************
    * availableMemPool
    ******************************************************************************/
    uint32_t SupervisedTarget::availableMemPool()
    {
      return memPool.getNumberOfFreeBlocks();
    }

    /******************************************************************************
    * getWriteCrossCompareMaxSizeSubClass
    ******************************************************************************/
    uint32_t SupervisedTarget::getWriteCrossCompareMaxSizeSubClass() const
    {
      // Se below in writeCrossCompareSubClass for the sizes...
      return 4U + 4U + 4U + 4U + 4U + 4U + 4U + 4U + 4U + 4U;
    }

    /******************************************************************************
    * writeCrossCompareSubClass
    ******************************************************************************/
    void SupervisedTarget::writeCrossCompareSubClass(VFW_Buffer * const buffer) const
    {
      // Write attributes...
      vfwPutU32(buffer, ceilingSpeed);
      vfwPutI32(buffer, gradient);
      vfwPutU32(buffer, firstWarningSpeed);
      vfwPutU32(buffer, secondWarningSpeed);
      vfwPutU32(buffer, sbSpeed);
      vfwPutU32(buffer, ebSpeed);
      vfwPutU32(buffer, fwCeilingSpeed);
      vfwPutU32(buffer, swCeilingSpeed);
      vfwPutU32(buffer, sbCeilingSpeed);
      vfwPutU32(buffer, ebCeilingSpeed);
    }

    /******************************************************************************
    * isTargetPassed
    ******************************************************************************/
    //lint -stack(ATP::DS::SupervisedTarget::isTargetPassed(32)) Not recursive, since it doesn't call this->isTargetPassed()
    bool SupervisedTarget::isTargetPassed() const
    {
      bool retVal = BaseTarget::isTargetPassed();

      if(type == SMSpeedRestrictionTarget)
      {
        //if SM speed target is passed set the speed restriction in targets.
        if(retVal)
        {
          AbstractTargets::corePtr()->setSMSpeedRestriction(AbstractConfig::corePtr()->getReleaseSpeed());
        }

        //SM target is deleted once parent is passed.
        retVal = (parentTarget->isTargetPassed());
      }

      return retVal;
    }

    /******************************************************************************
    * isPassedTargetHandled
    ******************************************************************************/
    bool SupervisedTarget::isPassedTargetHandled() const
    {
      bool handleTarget = true;

      switch(type)
      {
        case Undefined:
        {
          break;
        }

        case GradientSupvTarget:
        {
          AbstractTargets::corePtr()->setCurGradient(gradient);
          break;
        }

        case FWSpeedTarget:
        {
          AbstractTargets::corePtr()->setCurGradient(gradient);
          AbstractTargets::corePtr()->setCurrFwCeilingSpeed(fwCeilingSpeed);
          break;
        }

        case SWSpeedTarget:
        {
          AbstractTargets::corePtr()->setCurGradient(gradient);
          AbstractTargets::corePtr()->setCurrSwCeilingSpeed(swCeilingSpeed);
          break;
        }

        case SBSpeedTarget:
        {
          AbstractTargets::corePtr()->setCurGradient(gradient);
          AbstractTargets::corePtr()->setCurrSbCeilingSpeed(sbCeilingSpeed);
          break;
        }

        case EBSpeedTarget:
        {
          AbstractTargets::corePtr()->setCurGradient(gradient);
          AbstractTargets::corePtr()->setCurrEbCeilingSpeed(ebCeilingSpeed);
          break;
        }

        case FWPrimaryTarget:
        {
          AbstractTargets::corePtr()->setCurGradient(gradient);
          AbstractTargets::corePtr()->setCurrFwCeilingSpeed(fwCeilingSpeed);
          break;
        }

        case SWPrimaryTarget:
        {
          AbstractTargets::corePtr()->setCurGradient(gradient);
          AbstractTargets::corePtr()->setCurrFwCeilingSpeed(fwCeilingSpeed);
          AbstractTargets::corePtr()->setCurrSwCeilingSpeed(swCeilingSpeed);
          break;
        }

        case SBPrimaryTarget:
        {
          AbstractTargets::corePtr()->setCurGradient(gradient);
          AbstractTargets::corePtr()->setCurrFwCeilingSpeed(fwCeilingSpeed);
          AbstractTargets::corePtr()->setCurrSwCeilingSpeed(swCeilingSpeed);
          AbstractTargets::corePtr()->setCurrSbCeilingSpeed(sbCeilingSpeed);
          break;
        }

        case EBPrimaryTarget:
        case SpeedIncreaseTarget:
        {
          AbstractTargets::corePtr()->setCurGradient(gradient);
          AbstractTargets::corePtr()->setCurrFwCeilingSpeed(fwCeilingSpeed);
          AbstractTargets::corePtr()->setCurrSwCeilingSpeed(swCeilingSpeed);
          AbstractTargets::corePtr()->setCurrEbCeilingSpeed(ebCeilingSpeed);
          AbstractTargets::corePtr()->setCurrSbCeilingSpeed(sbCeilingSpeed);
          break;
        }

        case SMSpeedRestrictionTarget:
        {
          //The parent is passed so reset the speed restriction.
          AbstractTargets::corePtr()->setSMSpeedRestriction(0U);
          break;
        }

        default:
        {
          handleTarget = false;
          break;
        }

      }

      return handleTarget;

    }

    /******************************************************************************
    * getCeilingSpeed
    ******************************************************************************/
    uint32_t SupervisedTarget::getCeilingSpeed(void) const
    {
      return ceilingSpeed;
    }

    /******************************************************************************
    * getGradient
    ******************************************************************************/
    int8_t SupervisedTarget::getGradient(void) const
    {
      return static_cast<int8_t>(gradient);
    }

    /******************************************************************************
    * setCeilingSpeed
    ******************************************************************************/
    void SupervisedTarget::setCeilingSpeed(const uint32_t currentCeilingSpeed)
    {
      ceilingSpeed = currentCeilingSpeed;
    }

    /******************************************************************************
    * setGradient
    ******************************************************************************/
    void SupervisedTarget::setGradient(const int32_t currentGradient)
    {
      gradient = currentGradient;
      isValidGradient = true;
    }

    /******************************************************************************
    * getFirstWarningSpeed
    ******************************************************************************/
    uint32_t SupervisedTarget::getFirstWarningSpeed(void) const
    {
      return firstWarningSpeed;
    }

    /******************************************************************************
    * setFirstWarningSpeed
    ******************************************************************************/
    void SupervisedTarget::setFirstWarningSpeed(const uint32_t firstWarnSpeed)
    {
      firstWarningSpeed = firstWarnSpeed;
    }

    /******************************************************************************
    * getSecondWarningSpeed
    ******************************************************************************/
    uint32_t SupervisedTarget::getSecondWarningSpeed(void) const
    {
      return secondWarningSpeed;
    }

    /******************************************************************************
    * setSecondWarningSpeed
    ******************************************************************************/
    void SupervisedTarget::setSecondWarningSpeed(const uint32_t secondWarnSpeed)
    {
      secondWarningSpeed = secondWarnSpeed;
    }

    /******************************************************************************
    * getSBSpeed
    ******************************************************************************/
    uint32_t SupervisedTarget::getSBSpeed(void) const
    {
      return sbSpeed;
    }

    /******************************************************************************
    * setSBSpeed
    ******************************************************************************/
    void SupervisedTarget::setSBSpeed(const uint32_t serviceBrakeSpeed)
    {
      sbSpeed = serviceBrakeSpeed;
    }

    /******************************************************************************
    * getEBSpeed
    ******************************************************************************/
    uint32_t SupervisedTarget::getEBSpeed(void) const
    {
      return ebSpeed;
    }

    /******************************************************************************
    * setEBSpeed
    ******************************************************************************/
    void SupervisedTarget::setEBSpeed(const uint32_t emergencyBrakeSpeed)
    {
      ebSpeed = emergencyBrakeSpeed;
    }

    /******************************************************************************
    * getCeilingSpeed
    ******************************************************************************/
    uint32_t SupervisedTarget::getFwCeilingSpeed(void) const
    {
      return fwCeilingSpeed;
    }

    /******************************************************************************
    * getCeilingSpeed
    ******************************************************************************/
    uint32_t SupervisedTarget::getSwCeilingSpeed(void) const
    {
      return swCeilingSpeed;
    }

    /******************************************************************************
    * getCeilingSpeed
    ******************************************************************************/
    uint32_t SupervisedTarget::getSbCeilingSpeed(void) const
    {
      return sbCeilingSpeed;
    }

    /******************************************************************************
    * getCeilingSpeed
    ******************************************************************************/
    uint32_t SupervisedTarget::getEbCeilingSpeed(void) const
    {
      return ebCeilingSpeed;
    }

    /******************************************************************************
    * setCeilingSpeed
    ******************************************************************************/
    void SupervisedTarget::setFwCeilingSpeed(const uint32_t fwCS)
    {
      fwCeilingSpeed = fwCS;
    }

    /******************************************************************************
    * setCeilingSpeed
    ******************************************************************************/
    void SupervisedTarget::setSwCeilingSpeed(const uint32_t swCS)
    {
      swCeilingSpeed = swCS;
    }

    /******************************************************************************
    * setCeilingSpeed
    ******************************************************************************/
    void SupervisedTarget::setSbCeilingSpeed(const uint32_t sbCS)
    {
      sbCeilingSpeed = sbCS;
    }

    /******************************************************************************
    * setCeilingSpeed
    ******************************************************************************/
    void SupervisedTarget::setEbCeilingSpeed(const uint32_t ebCS)
    {
      ebCeilingSpeed = ebCS;
    }

    /******************************************************************************
    * getParentTarget
    ******************************************************************************/
    BaseTarget* SupervisedTarget::getParentTarget()
    {
      return parentTarget;
    }

    /******************************************************************************
    * isGradientValid
    ******************************************************************************/
    bool SupervisedTarget::isGradientValid(void) const
    {
      return isValidGradient;
    }

  }
}
