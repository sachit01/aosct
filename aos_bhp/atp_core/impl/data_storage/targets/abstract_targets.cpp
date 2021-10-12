/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of AbstractTargets
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-16    lantback    Created
* 2016-04-21    lantback    Implemented corePtr()
* 2016-04-22    lantback    Added component type
* 2016-07-27    akushwah    Initial Implementation
* 2016-08-26    bhidaji     In The method removePassed changed rearPos to frontPos
* 2016-09-06    akushwah    Implementation after re-design
* 2016-09-19    akushwah    Corrected Init function
* 2016-09-21    bhidaji     gradient changed from uint32_t to int32_t
* 2016-09-23    arastogi    Deleting should compare whole target and not just id
*                           Added consoleCall.
* 2016-09-27    arastogi    Added check for ma margin in removePassed
* 2016-09-29    bhidaji     Changed travelDirection to supposed travel direction
*                           Added logic for targetListChanged.
*                           Added getSupposedTravelDir(), getTargetListChanged(), isTargetListEmpty()
*                           Added setConditionalTargetSupervise()
* 2016-10-14    arastogi    Fixed removing target from list only if they are deleted.
* 2016-10-18    arastogi    Fixed checking for passed targets in reverse
* 2016-11-03    spandita    Updated with redesign of targets
* 2017-06-26    skothiya    Updated to rename and modify removePassedTargets() method
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <cstdio>
#include "abstract_config.hpp"
#include "abstract_targets.hpp"
#include "abstract_event_handler.hpp"
#include "abstract_console.hpp"
#include "abstract_odometry.hpp"
#include "dmi_event_codes.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include "abstract_mode_control.hpp"
#include "abstract_analyzer_if.hpp"
#include <vfw_checkpoints.h>
#include "abstract_brake.hpp"
#include "abstract_targets_event_ids.hpp"
#include "abstract_tsetup.hpp"
#include "atc_math.hpp"

/******************************************************************************
* LINT SUPPRESSIONS
******************************************************************************/
//lint -esym(586,snprintf) snprintf is needed here

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

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
    AbstractTargets::AbstractTargets() : ATC::ProcComponent(atpTargetsId, "Targets", "TA"),
      // creating different set of objects for different type of events
      invalidGradientMemAllocation(ATC::Event::createSafetyHaltEvent(atpTargetsId, ATC::CoreContainer, eventIdInvalidMemForGrad,
        ATC::NoEB, DMICom::targetsInternalFailure, "Invalid Memory Allocation for GradientTarget")),
      invalidPrimaryMemAllocation(ATC::Event::createSafetyHaltEvent(atpTargetsId, ATC::CoreContainer, eventIdInvalidMemForPrim,
        ATC::NoEB, DMICom::targetsInternalFailure, "Invalid Memory Allocation for PrimaryTarget")),
      invalidSpeedMemAllocation(ATC::Event::createSafetyHaltEvent(atpTargetsId, ATC::CoreContainer, eventIdInvalidMemForSpeed,
        ATC::NoEB, DMICom::targetsInternalFailure, "Invalid Memory Allocation for SpeedTarget")),
      invalidSupervisedMemAllocation(ATC::Event::createSafetyHaltEvent(atpTargetsId, ATC::CoreContainer, eventIdInvalidMemForSupervised,
        ATC::NoEB, DMICom::targetsInternalFailure, "Invalid Memory Allocation for SupervisedTarget")),
      invalidTrackDataMemAllocation(ATC::Event::createSafetyHaltEvent(atpTargetsId, ATC::CoreContainer, eventIdInvalidMemForTrkData,
        ATC::NoEB, DMICom::targetsInternalFailure, "Invalid Memory Allocation for TrackDataItemTarget")),
      invalidKeepTrackDataMemAllocation(ATC::Event::createSafetyHaltEvent(atpTargetsId, ATC::CoreContainer, eventIdInvalidMemForKeepTrkData,
        ATC::NoEB, DMICom::targetsInternalFailure, "Invalid Memory Allocation for KeepTrackDataTarget")),
      invalidDirection(ATC::Event::createSafetyHaltEvent(atpTargetsId, ATC::CoreContainer, eventIdInvalidDirection,
        ATC::NoEB, DMICom::targetsInternalFailure, "Dynamic cast failure in target")),
      targetListFullError(ATC::Event::createSafetyHaltEvent(atpTargetsId, ATC::CoreContainer, eventIdTargetListFull,
        ATC::NoEB, DMICom::targetsTrgtListFull, "Abstract Target List is Full")),
      initNotDone(ATC::Event::createSafetyHaltEvent(atpTargetsId, ATC::CoreContainer, eventIdMemNotSet, ATC::NoEB,
        DMICom::targetsInternalFailure, "Memory for the Initialization not set properly")),
      baseTargetNULLError(ATC::Event::createSafetyHaltEvent(atpTargetsId, ATC::CoreContainer, eventIdNullPointer,
        ATC::NoEB, DMICom::targetsInternalFailure, "Base Target has NULL Pointer")),
      unknownTargetDirectionError(ATC::Event::createSafetyHaltEvent(atpTargetsId, ATC::CoreContainer, eventIdUnknownDir,
        ATC::NoEB, DMICom::targetsInternalFailure, "Unknown Direction while adding Target to the List, Target ID:", true)),
      inconsistencyTargetTrack(ATC::Event::createSafetyHaltEvent(atpTargetsId, ATC::CoreContainer, eventIdInconsistencyError,
        ATC::NoEB, DMICom::targetsInconsistentTrgt, "Inconsistency between target/track storage")),
      sameTargetIdError(ATC::Event::createSafetyHaltEvent(atpTargetsId, ATC::CoreContainer, eventIdSameTargetId,
        ATC::NoEB, DMICom::targetsSameTrgtId, "Same target id found, Target ID:", true)),
      contSizeExceeded(ATC::Event::createSafetyHaltEvent(atpTargetsId, ATC::CoreContainer, eventIdExceededContSize,
        ATC::NoEB, 0x0U, "Exceeding internal container size!")),
      twoSupGradAtSamePos(ATC::Event::createSafetyHaltEvent(atpTargetsId, ATC::CoreContainer, eventIdTwoSupGradSamePos,
        ATC::NoEB, 0x0U, "Supervise gradient is calculate at position of another supervised gradient!")),
      nullPointerAcess(ATC::Event::createSafetyHaltEvent(atpModeControlId, ATC::CoreContainer, eventIdNullPointerAcess,
        ATC::NoEB, DMICom::nullPointerAccess, "NULL Pointer access")),
      odometerInvalidTargetActive(false),
      freeRollingTargetActive(false),
      coreInitDone(false),
      supposedTravelDir(DirUndefined), //Initializing the travel direction as forward
      targetListChanged(false),
      targetListReversed(false),
      reachedPrimaryTarget(false),
      curCeilingSpeed(0U),
      curTrackGradient(0),
      curGradient(0),
      smSpeedRestriction(0U),
      delTargetAtStandStill(false),
      currentSafetyMargin(0U),
      isTargetDeletedInList(false),
      freeBlocksPrim(0U),
      freeBlocksGrad(0U),
      freeBlocksSpeed(0U),
      freeBlocksTDI(0U),
      freeBlocksSupervised(0U),
      curFWCeilingSpeed(0U),
      curSWCeilingSpeed(0U),
      curSBCeilingSpeed(0U),
      curEBCeilingSpeed(0U),
      adhesionValue(100U)
    {
      if (coreTargetsInstancePtr != 0)
      {
        // Error handler
        ATC::aosHalt(__FILE__, __LINE__, "Target Constructor already instantiated");
      }

      // Setup single instance pointer for core access
      coreTargetsInstancePtr = this;
    }

    /******************************************************************************
    * init
    ******************************************************************************/
    bool AbstractTargets::init(void)
    {
      if (!coreInitDone)
      {
        //Initialize the memory for the primary Target 
        bool initDone1 = PrimaryTarget::initMemPoolSize(maxNumberOfPrimaryTargets);
        //Initialize the memory for the Gradient Target
        bool initDone2 = GradientTarget::initMemPoolSize(maxNumberOfGradientTargets);
        //Initialize the memory for the Speed Target
        bool initDone3 = SpeedTarget::initMemPoolSize(maxNumberOfSpeedTargets);
        //Initialize the memory for the TrackDataItem Target
        bool initDone4 = TrackDataItemTarget::initMemPoolSize(maxNumberOfTrackDataItemTargets);
        //Initialize the memory for the Supervised Target
        bool initDone5 = SupervisedTarget::initMemPoolSize(maxNumberOfSupvTotalTargets);

        ATC::AbstractAnalyzerIF* const aif = ATC::AbstractAnalyzerIF::corePtr();

        bool regMeasureAIF = aif->registerMeasurement("Track gradient", "gradient", "0.1%", -100, +100, &curTrackGradient);
        regMeasureAIF = aif->registerMeasurement("gradient", "gradient", "0.1%", -100, +100, &curGradient) && regMeasureAIF;
        regMeasureAIF = aif->registerMeasurement("FWCeilingSpeed", "Current First warning Ceiling speed", "cm/s", 0U, 5000U,
          &curFWCeilingSpeed) && regMeasureAIF;
        regMeasureAIF = aif->registerMeasurement("SWCeilingSpeed", "Current Second warning Ceiling speed", "cm/s", 0U, 5000U,
          &curSWCeilingSpeed) && regMeasureAIF;
        regMeasureAIF = aif->registerMeasurement("SBCeilingSpeed", "Current Service brake Ceiling speed", "cm/s", 0U, 5000U,
          &curSBCeilingSpeed) && regMeasureAIF;
        regMeasureAIF = aif->registerMeasurement("EBCeilingSpeed", "Current Emergency brake Ceiling speed", "cm/s", 0U, 5000U,
          &curEBCeilingSpeed) && regMeasureAIF;

        if (!regMeasureAIF)
        {
          writeToLog(ATC::BriefLog, "Register measurement failed for analyzerIF in targets", __FILE__, __LINE__);
        }

        //Safety Margin
        currentSafetyMargin = 0U; // Safety margin will be set by first MA from scratch

        if (initDone1 && initDone2 && initDone3 && initDone4 && initDone5)
        {
          initCrossCompare();
          coreInitDone = true;
        }
        else
        {
          // Error handler
          ATC::AbstractEventHandler::corePtr()->reportEvent(initNotDone, __FILE__
            , __LINE__);
        }
      }

      return coreInitDone;
    }

    /******************************************************************************
    * run
    ******************************************************************************/
    void AbstractTargets::run(void)
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "TA_run");

      //Reset the flag
      isTargetDeletedInList = false;
      // reset target list changed
      targetListChanged = false;
      targetListReversed = false;

      if (Pos::AbstractOdometry::corePtr()->isTrainStandStill() && delTargetAtStandStill)
      {
        removeAll();
        delTargetAtStandStill = false;
      }
      // Manage Emergency Stop Active signal 
      manageEMSInput();

      // reset status  changed in conditional targets
      MaTargetIterator it = maTargetList.begin();
      for (it = maTargetList.begin(); (it != maTargetList.end()); ++it)
      {
        if (BaseTarget::ConditionalTarget == (*it)->getTargetType())
        {
          ConditionalTarget* condTarg = ATC::dynamicCast<BaseTarget*, ConditionalTarget*>(*it, __FILE__, __LINE__);
          condTarg->resetStatusChanged();
        }
      }

      const bool isModeLocation = (ATPModeLocation == Kernel::AbstractModeControl::corePtr()->getCurrentMode());
      const bool isIdling = Kernel::AbstractModeControl::corePtr()->getIdleState();

      //reset the speed restriction if the train is idling or mode changed to location
      if(isIdling || isModeLocation)
      {
        smSpeedRestriction = 0U;
      }

      // Save previous values for the free memory-blocks counters. 
      oldFreeBlocksPrim = freeBlocksPrim;
      oldFreeBlocksGrad = freeBlocksGrad;
      oldFreeBlocksSpeed = freeBlocksSpeed;
      oldFreeBlocksTDI = freeBlocksTDI;
      oldFreeBlocksSupervised = freeBlocksSupervised;

      // Update counters for free memory-blocks in the memory-pool.
      freeBlocksPrim = PrimaryTarget::availableMemPool();
      freeBlocksGrad = GradientTarget::availableMemPool();
      freeBlocksSpeed = SpeedTarget::availableMemPool();
      freeBlocksTDI = TrackDataItemTarget::availableMemPool();
      freeBlocksSupervised = SupervisedTarget::availableMemPool();

      // Log according to trace-level
      veryDetailedLog();
    }

    /******************************************************************************
    * addTarget for PrimaryTarget
    ******************************************************************************/
    void AbstractTargets::addTarget(PrimaryTarget const &t)
    {
      PrimaryTarget *pTarget = new PrimaryTarget(t);
      insertMaTargetInList(pTarget);

      const bool isModeLocation = (ATPModeLocation == Kernel::AbstractModeControl::corePtr()->getCurrentMode());
      if (isModeLocation)
      { // addTarget will be called when changing the location borders in Location mode
        // Always consider primary target to be reached (for evaluation of MA Timeout) if dropping out of Location
        // because of Stop Train or Emergency Alert
        reachedPrimaryTarget = true;
      }
      else
      {
        reachedPrimaryTarget = false;
      }
    }

    /******************************************************************************
    * addTarget for SpeedTarget
    ******************************************************************************/
    void AbstractTargets::addTarget(SpeedTarget const &t)
    {
      SpeedTarget *sTarget = new SpeedTarget(t);
      insertMaTargetInList(sTarget);
    }

    /******************************************************************************
    * addTarget for GradientTarget
    ******************************************************************************/
    void AbstractTargets::addTarget(GradientTarget const &t)
    {
      GradientTarget *gTarget = new GradientTarget(t);
      insertMaTargetInList(gTarget);
    }

    /******************************************************************************
    * addTarget for TrackDataItemTarget
    ******************************************************************************/
    void AbstractTargets::addTarget(TrackDataItemTarget const &t)
    {
      TrackDataItemTarget *tTarget = new TrackDataItemTarget(t);
      insertMaTargetInList(tTarget);
    }

    /******************************************************************************
    * addTarget for SupervisedTarget
    ******************************************************************************/
    void AbstractTargets::addTarget(const SupervisedTarget &t)
    {
      SupervisedTarget *tTarget = new SupervisedTarget(t);
      insertSupvTargetInList(tTarget);
    }

    /******************************************************************************
    * delTarget
    ******************************************************************************/
    bool AbstractTargets::delTarget(const BaseTarget* const bTarget)
    {
      bool ret = false;

      if (bTarget != static_cast<const BaseTarget*>(NULL))
      {
        if (dynamic_cast<const SupervisedTarget*>(bTarget) != NULL) //lint !e929 dynamic_cast is ok
        {
          for (SupervisedTargetIterator it = supvTargetList.begin(); it != supvTargetList.end(); ++it)
          {
            if ((*it)->getTargetId() == bTarget->getTargetId())
            {
              ret = true;

              delete *it;
              static_cast<void>(supvTargetList.erase(it));
              isTargetDeletedInList = true;
              break;
            }
          }
        }
        else
        {
          for (MaTargetIterator it = maTargetList.begin(); it != maTargetList.end(); ++it)
          {
            if ((*it)->getTargetId() == bTarget->getTargetId())
            {
              ret = true;
              delAllRelatedSupvTarget(*it);

              delete *it;
              static_cast<void>(maTargetList.erase(it));
              isTargetDeletedInList = true;
              break;
            }
          }
        }
      }

      return ret;
    }

    /******************************************************************************
    * delete for BaseTarget
    ******************************************************************************/
    bool AbstractTargets::delTarget(MaTargetIterator &it)
    {
      bool ret = false;
      const BaseTarget* const target = *it;
      
      if (target != static_cast<BaseTarget*>(NULL))
      {
        if (it != maTargetList.end())
        {
          //delete supervised targets if *it is a maTarget
          delAllRelatedSupvTarget(*it);

          //delete target
          delete target;
          it = maTargetList.erase(it);
          ret = true;
          isTargetDeletedInList = true;
        }
      }

      return ret;
    }

    /******************************************************************************
    * delete for SupervisedTarget
    ******************************************************************************/
    bool AbstractTargets::delTarget(SupervisedTargetIterator &it)
    {
      bool ret = false;
      const SupervisedTarget* const target = *it;

      if (target != static_cast<SupervisedTarget*>(NULL))
      {
        if (it != supvTargetList.end())
        {
          //delete target
          delete target;
          it = supvTargetList.erase(it);
          ret = true;
          isTargetDeletedInList = true;
        }
      }

      return ret;
    }

    /******************************************************************************
    * delAllRelatedSupvTarget
    ******************************************************************************/
    void AbstractTargets::delAllRelatedSupvTarget(const BaseTarget* const bTarget)
    {
      if (bTarget != static_cast<BaseTarget*>(NULL))
      {
        if(bTarget->getTargetType() != BaseTarget::SupervisedTarget)
        {
          SupervisedTargetIterator it = supvTargetList.begin();
          while (it != supvTargetList.end())
          {
            if ((*it)->getParentTarget()->getTargetId() == bTarget->getTargetId())
            {
              delete(*it);
              it = supvTargetList.erase(it);
            }
            else
            {
              ++it;
            }
          }
        }
      }
    }

    /******************************************************************************
    * targetListFitsVolume
    ******************************************************************************/
    bool AbstractTargets::targetListFitsVolume(const size_t size) const
    {
      return ((maTargetList.size() + size) <= maTargetList.maximumSize());
    }

    /******************************************************************************
    * supvTargetListFitsVolume
    ******************************************************************************/
    bool AbstractTargets::supvTargetListFitsVolume(const uint32_t size) const
    {
      return ((supvTargetList.size() + size) <= supvTargetList.maximumSize());
    }

    /******************************************************************************
    * removePassedTargets
    ******************************************************************************/
    void AbstractTargets::removePassedTargets()
    {
      bool continueLoop = true;
      //remove passed supervised targets
      SupervisedTargetIterator supvIt = supvTargetList.begin();

      while (supvIt != supvTargetList.end())
      {
        SupervisedTarget* const supTarget = *supvIt;
        if (supTarget != static_cast<supervisedTargetPtr>(NULL))
        {
          bool targetWasDeleted = false;

          //Check if target passed
          if (supTarget->isTargetPassed())
          {
            //Performing operation before removing the target and checking if target can not be removed in some conditions
            if (supTarget->isPassedTargetHandled())
            {
              //if the SM speed restriction is removed, trigger recalculate of the curve speeds
              if(SupervisedTarget::SMSpeedRestrictionTarget == supTarget->getSupervisedTargetType())
              {
                targetListChanged = true;
              }
              //Primary supervised targets should be deleted when the MA Primary target is deleted.
              if (BaseTarget::PrimaryTarget != supTarget->getParentTarget()->getTargetType())
              {
                //delTarget remove the target and also remove the target from targetlist
                if (delTarget(supvIt))
                {
                  targetWasDeleted = true;
                }
              }
            }
          }

          if (!targetWasDeleted)
          {
            ++supvIt;
          }
        }
        else
        {
          //Raise safety Halt for NULL pointer
          ATC::AbstractEventHandler::corePtr()->reportEvent(nullPointerAcess, __FILE__
            , __LINE__);
        }
      }

      MaTargetIterator it = getMATargetIter();
      while ((it != getMATargetIterEnd()) && continueLoop)
      {
        const maTargetPtr pTarget = (*it);
        if (pTarget != static_cast<maTargetPtr>(NULL))
        {
          //Check if target passed 
          if (pTarget->isTargetPassed())
          {
            //Performing operation before removing the target and checking if target can not be removed in some conditions
            if (pTarget->isPassedTargetHandled())
            {
              if (BaseTarget::PrimaryTarget == pTarget->getTargetType())
              {
                reachedPrimaryTarget = true;
                removeAll();
                break;
              }
              else
              {
                //delTarget remove the target and also remove the target from targetlist
                if (delTarget(it))
                {
                  //Do nothing
                }
                else
                {
                  ++it;
                }

              }
            }
            else
            {
              ++it;
            }
          }
          else
          {
            //Targets are stored in increasing order of position/odoval on track
            //Therefore to optimize the method if a target in the supposed travel direction is not passed the current odoval 
            //then no need to check further track list
            //So breaking from the loop, only gradient targets get removed when rear end passed
            bool isNotGradientTarget = (pTarget->getTargetType() != BaseTarget::GradientTarget);
            bool isCorrectDir = (pTarget->getDirection() == getSupposedTravelDir());
            if (isNotGradientTarget && isCorrectDir)
            {
              continueLoop = false;
            }
            else
            {
              ++it;
            }
          }
        }
        else
        {
          //Raise safety Halt for NULL pointer
          ATC::AbstractEventHandler::corePtr()->reportEvent(nullPointerAcess, __FILE__
            , __LINE__);
        }
      }//End of while
    }

    /******************************************************************************
    * getFreeBlocksSpeed
    ******************************************************************************/
    uint32_t AbstractTargets::getFreeBlocksSpeed() const
    {
      return freeBlocksSpeed;
    }

    /******************************************************************************
    * getFreeBlocksTDI
    ******************************************************************************/
    uint32_t AbstractTargets::getFreeBlocksTDI() const
    {
      return freeBlocksTDI;
    }

    /******************************************************************************
    * getFreeBlocksGrad
    ******************************************************************************/
    uint32_t AbstractTargets::getFreeBlocksGrad() const
    {
      return freeBlocksGrad;
    }

    /******************************************************************************
    * removeAll
    ******************************************************************************/
    void AbstractTargets::removeAll()
    {
      //Delete the full target list 
      for (MaTargetIterator it = maTargetList.begin(); it != maTargetList.end(); ++it)
      {
        delete (*it);
        isTargetDeletedInList = true;
      }

      //Removes all targets from the list, and leaving the list with a size of 0 
      maTargetList.clear();

      //Delete the full supv target list
      for (SupervisedTargetIterator it = supvTargetList.begin(); it != supvTargetList.end(); ++it)
      {
        delete(*it);
        isTargetDeletedInList = true;
      }
      //Removes all targets from the list, and leaving the list with a size of 0
      supvTargetList.clear();

      //set the current G/B/C to default
      curTrackGradient = 0;
      curCeilingSpeed = 0U;
      curFWCeilingSpeed = 0U;
      curSWCeilingSpeed = 0U;
      curSBCeilingSpeed = 0U;
      curEBCeilingSpeed = 0U;

      smSpeedRestriction = 0U;
    }

    /******************************************************************************
    * removeAllSupervisedTargets
    ******************************************************************************/
    void AbstractTargets::removeAllSupervisedTargets()
    {
      //Delete the full supv target list
      for (SupervisedTargetIterator it = supvTargetList.begin(); it != supvTargetList.end(); ++it)
      {
        delete(*it);
        isTargetDeletedInList = true;
      }
      //Removes all targets from the list, and leaving the list with a size of 0
      supvTargetList.clear();

      smSpeedRestriction = 0U;
    }

    /******************************************************************************
    * setSupposedTravelDir
    ******************************************************************************/
    void AbstractTargets::setSupposedTravelDir(const TravelDir stDir)
    {
      if ((supposedTravelDir != stDir) && (supposedTravelDir != DirUndefined))
      {
        if (!maTargetList.empty())
        {
          if ((DirBoth != stDir) && (DirNone != stDir) && (DirUndefined != stDir))
          {
            //is mode location?
            const bool isModeLocation = (ATPModeLocation == Kernel::AbstractModeControl::corePtr()->getCurrentMode());
            const bool isFreeRooling = Kernel::AbstractModeControl::corePtr()->getFreeRolling();
            const bool isStandStill = Pos::AbstractOdometry::corePtr()->isTrainStandStill();

            if (isModeLocation && (isFreeRooling || isStandStill))
            {
              targetListReversed = true;
              //reverse the lists
              maTargetList.reverse();
              if (!supvTargetList.empty())
              {
                supvTargetList.reverse();
              }
            }
          }
        }
      }
      supposedTravelDir = stDir;
    }

    /******************************************************************************
    * setAdhesionValue
    ******************************************************************************/
    void AbstractTargets::setAdhesionValue(const uint8_t adhesionV)
    {
      adhesionValue = adhesionV;
    }
    
    /******************************************************************************
    * getAdhesionValue
    ******************************************************************************/
    uint8_t AbstractTargets::getAdhesionValue(void) const
    {
      return adhesionValue;
    }

    /******************************************************************************
    * getSupposedTravelDir
    ******************************************************************************/
    TravelDir AbstractTargets::getSupposedTravelDir(void) const
    {
      return supposedTravelDir;
    }

    /******************************************************************************
    * getTargetListChanged
    ******************************************************************************/
    bool AbstractTargets::getTargetListChanged(void) const
    {
      return targetListChanged;
    }

    /******************************************************************************
    * isTargetListReversed
    ******************************************************************************/
    bool AbstractTargets::isTargetListReversed(void) const
    {
      return targetListReversed;
    }

    /******************************************************************************
    * isPrimaryTargetReached
    ******************************************************************************/
    bool AbstractTargets::isPrimaryTargetReached() const
    {
      return reachedPrimaryTarget;
    }

    /******************************************************************************
    * getMATargetIter
    ******************************************************************************/
    MaTargetIterator AbstractTargets::getMATargetIter()
    {
      MaTargetIterator iteratorBegin = maTargetList.begin();
      return iteratorBegin;
    }

    /******************************************************************************
    * getConstMATargetIter
    ******************************************************************************/
    ConstMaTargetIterator AbstractTargets::getConstMATargetIter() const
    {
      return maTargetList.begin();
    }

    /******************************************************************************
    * getMATargetIterEnd
    ******************************************************************************/
    MaTargetIterator AbstractTargets::getMATargetIterEnd()
    {
      MaTargetIterator iteratorEnd = maTargetList.end();
      return iteratorEnd;
    }

    /******************************************************************************
    * getConstMATargetIterEnd
    ******************************************************************************/
    ConstMaTargetIterator AbstractTargets::getConstMATargetIterEnd() const
    {
      return maTargetList.end();
    }

    /******************************************************************************
    * isMATargetListEmpty
    ******************************************************************************/
    bool AbstractTargets::isMATargetListEmpty() const
    {
      return maTargetList.empty();
    }

    /******************************************************************************
    * getSupervisedRevTargetIterEnd
    ******************************************************************************/
    SupervisedTargetRevIterator AbstractTargets::getSupervisedRevTargetIterEnd()
    {
      SupervisedTargetRevIterator revIteratorEnd = supvTargetList.rend();
      return revIteratorEnd;
    }

    /******************************************************************************
    * getSupervisedTargetIter
    ******************************************************************************/
    SupervisedTargetIterator AbstractTargets::getSupervisedTargetIter()
    {
      SupervisedTargetIterator iteratorBegin = supvTargetList.begin();
      return iteratorBegin;
    }

    /******************************************************************************
    * getSupervisedTargetIterEnd
    ******************************************************************************/
    SupervisedTargetIterator AbstractTargets::getSupervisedTargetIterEnd()
    {
      SupervisedTargetIterator iteratorEnd = supvTargetList.end();
      return iteratorEnd;
    }

    /******************************************************************************
    * getSupervisedRevTargetIter
    ******************************************************************************/
    SupervisedTargetRevIterator AbstractTargets::getSupervisedRevTargetIter()
    {
      SupervisedTargetRevIterator revIteratorBegin = supvTargetList.rbegin();
      return revIteratorBegin;
    }

    /******************************************************************************
    * isSupervisedTargetListEmpty
    ******************************************************************************/
    bool AbstractTargets::isSupervisedTargetListEmpty() const
    {
      return supvTargetList.empty();
    }

    /******************************************************************************
    * delTargetsInRange
    ******************************************************************************/
    void AbstractTargets::delTargetsInRange(const OdoPosition startOdoPos, const OdoPosition endOdoPos)
    {
      MaTargetIterator targetIt = maTargetList.begin();

      // Iterate over all targets
      while (targetIt != maTargetList.end())
      {
        BaseTarget* const target = *targetIt;
        const OdoPosition odoVal = target->getOdometer();
        BaseTarget::CoreTargetType targetType = target->getTargetType();

        if (DirForward == supposedTravelDir)
        {
          if ((startOdoPos <= odoVal) &&
            (odoVal <= endOdoPos) && (BaseTarget::PrimaryTarget != targetType))
          {
            delAllRelatedSupvTarget(target);
            delete target;
            targetIt = maTargetList.erase(targetIt);
            isTargetDeletedInList = true;
          }
          else
          {
            ++targetIt;
          }
        }
        else if (DirReverse == supposedTravelDir)
        {
          if ((startOdoPos >= odoVal) &&
            (odoVal >= endOdoPos) && (BaseTarget::PrimaryTarget != targetType))
          {
            delAllRelatedSupvTarget(target);
            delete target;
            targetIt = maTargetList.erase(targetIt);
            isTargetDeletedInList = true;
          }
          else
          {
            ++targetIt;
          }
        }
        else
        {
          // We should always have a direction if we have a target to delete!
          ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDirection, __FILE__
            , __LINE__);
          break;
        }
      }
      
      //delete supervised targets whose Ma target is not deleted.
      SupervisedTargetIterator supvTargetIt = supvTargetList.begin();
      // iterate over all targets in supvTargetList
      for (supvTargetIt = supvTargetList.begin(); supvTargetIt != supvTargetList.end();)
      {
        const OdoPosition odoVal = (*supvTargetIt)->getOdometer();
        BaseTarget::CoreTargetType parentTargetType = (*supvTargetIt)->getParentTarget()->getTargetType();

        if (DirForward == supposedTravelDir)
        {
          if ((startOdoPos <= odoVal) &&
            (odoVal <= endOdoPos) && (BaseTarget::PrimaryTarget != parentTargetType))
          {
            delete(*supvTargetIt);
            supvTargetIt = supvTargetList.erase(supvTargetIt);
            isTargetDeletedInList = true;
          }
          else
          {
            ++supvTargetIt;
          }
        }
        else if (DirReverse == supposedTravelDir)
        {
          if ((startOdoPos >= odoVal) &&
            (odoVal >= endOdoPos) && (BaseTarget::PrimaryTarget != parentTargetType))
          {
            delete(*supvTargetIt);
            supvTargetIt = supvTargetList.erase(supvTargetIt);
            isTargetDeletedInList = true;
          }
          else
          {
            ++supvTargetIt;
          }
        }
        else
        {
          // We should always have a direction if we have a target to delete!
          ATC::AbstractEventHandler::corePtr()->reportEvent(invalidDirection, __FILE__
            , __LINE__);
          break;
        }
      }
    }

    /******************************************************************************
    * getPrimaryTarget
    ******************************************************************************/
    BaseTarget* AbstractTargets::getPrimaryTarget(TravelDir const trvDir)
    {
      BaseTarget* pTarget = static_cast<BaseTarget*>(NULL);
      MaTargetRevIterator targetIt = maTargetList.rbegin();
      for (; targetIt != maTargetList.rend(); ++targetIt)
      {
        if ((*targetIt)->getTargetType() == BaseTarget::PrimaryTarget)
        {
          if ((*targetIt)->getDirection() == trvDir)
          {
            pTarget = (*targetIt);
            break;
          }
        }
      }

      return pTarget;
    }

    /******************************************************************************
    *overloaded getPrimaryTarget
    ******************************************************************************/
    BaseTarget* AbstractTargets::getPrimaryTarget()
    {
      BaseTarget* pTarget = static_cast<BaseTarget*>(NULL);
      MaTargetRevIterator targetIt = maTargetList.rbegin();
      for (; targetIt != maTargetList.rend(); ++targetIt)
      {
        if ((*targetIt)->getTargetType() == BaseTarget::PrimaryTarget)
        {
          pTarget = (*targetIt);
          break;
        }
      }

      return pTarget;
    }
    /******************************************************************************
    * getLocationStartTarget
    ******************************************************************************/
    BaseTarget* AbstractTargets::getLocationStartTarget()
    {
      BaseTarget* pTarget = static_cast<BaseTarget*>(NULL);
      MaTargetRevIterator targetIt = maTargetList.rbegin();
      for (; targetIt != maTargetList.rend(); ++targetIt)
      {
        if ((*targetIt)->getTargetType() == BaseTarget::PrimaryTarget)
        {
          if (static_cast<uint8_t>(BaseTarget::LocationStartTargetType) == (*targetIt)->getRouteType())
          {
            pTarget = (*targetIt);
            break;
          }
        }
      }

      return pTarget;
    }

    /******************************************************************************
    * getLocationEndTarget
    ******************************************************************************/
    BaseTarget* AbstractTargets::getLocationEndTarget()
    {
      BaseTarget* pTarget = static_cast<BaseTarget*>(NULL);
      MaTargetRevIterator targetIt = maTargetList.rbegin();
      for (; targetIt != maTargetList.rend(); ++targetIt)
      {
        if ((*targetIt)->getTargetType() == BaseTarget::PrimaryTarget)
        {
          if (static_cast<uint8_t>(BaseTarget::LocationEndTargetType) == (*targetIt)->getRouteType())
          {
            pTarget = (*targetIt);
            break;
          }
        }
      }

      return pTarget;
    }

    /******************************************************************************
    * corePtr
    ******************************************************************************/
    AbstractTargets* AbstractTargets::corePtr(void)
    {
      return coreTargetsInstancePtr;
    }

    /******************************************************************************
    * insertMaTargetInList
    ******************************************************************************/
    void AbstractTargets::insertMaTargetInList(BaseTarget* const target)
    {
      bool ret = false;

      if (target == static_cast<BaseTarget*>(NULL))
      {
        //Report An Error for Null pointer
        ATC::AbstractEventHandler::corePtr()->reportEvent(baseTargetNULLError, __FILE__
          , __LINE__);
      }
      else if (maTargetList.full())
      {
        //Report An Error when Target List is Full
        ATC::AbstractEventHandler::corePtr()->reportEvent(targetListFullError, __FILE__
          , __LINE__);
      }
      else if (maTargetList.empty())
      {
        maTargetList.pushBack(target);
        ret = true;
      }
      else
      {
        ret = insertMaTargetInListInternal(target);
      }

      if (ret) // if a new target was successfully added
      {
        targetListChanged = true;
      }
    }

    /******************************************************************************
    * insertSupvTargetInList
    ******************************************************************************/
    void AbstractTargets::insertSupvTargetInList(SupervisedTarget* const target)
    {
      bool ret = false;

      if (target == static_cast<SupervisedTarget*>(NULL))
      {
        //Report An Error for Null pointer
        ATC::AbstractEventHandler::corePtr()->reportEvent(baseTargetNULLError, __FILE__
          , __LINE__);
      }
      else if (supvTargetList.full())
      {
        //Report An Error when Target List is Full
        ATC::AbstractEventHandler::corePtr()->reportEvent(targetListFullError, __FILE__
          , __LINE__);
      }
      else if (supvTargetList.empty())
      {
        supvTargetList.pushBack(target);
        ret = true;
      }
      else
      {
        ret = insertSupvTargetInListInternal(target);
      }

      if (ret) // if a new target was successfully added
      {
        targetListChanged = true;
      }
    }

    /******************************************************************************
    * insertSupvTargetInListInternal
    ******************************************************************************/
    bool AbstractTargets::insertSupvTargetInListInternal(SupervisedTarget * const supvTarg)
    {
      bool ret = false;
      bool targetAtSamePosFound = false;
      bool errorOccured = false;

      SupervisedTargetIterator odoInsertPositionIter = supvTargetList.end();
      SupervisedTargetIterator it = supvTargetList.begin();

      for (; it != supvTargetList.end(); ++it)
      {
        SupervisedTarget* const target = *it;
        if (target->getTargetId() == supvTarg->getTargetId())
        {
          //Safety halt
          //prepare the dynamic text to be send while reporting event.
          sameTargetIdError.setDynamicText(target->getTargetId());

          ATC::AbstractEventHandler::corePtr()->reportEvent(sameTargetIdError, __FILE__
            , __LINE__);
          errorOccured = true;
        }
        else
        {
          bool isTargetPosFound = false;
          if (DirForward == supposedTravelDir)
          {
            isTargetPosFound = (target->getOdometer() > supvTarg->getOdometer());
          }
          else if (DirReverse == supposedTravelDir)
          {
            isTargetPosFound = (target->getOdometer() < supvTarg->getOdometer());
          }
          else
          {
            //Report Error for unknown Direction while adding target
            //prepare the dynamic text to be send while reporting event.
            unknownTargetDirectionError.setDynamicText(target->getTargetId());
            ATC::AbstractEventHandler::corePtr()->reportEvent(unknownTargetDirectionError, __FILE__, __LINE__);
            errorOccured = true;
          }

          if (!errorOccured)
          {
            if (isTargetPosFound)
            {
              odoInsertPositionIter = it;
            }
            else if ((target->getOdometer() == supvTarg->getOdometer()))
            {
              //Supervised target of gradient can overlap with other supervised target.
              //In that case, the gradient should be updated on the other target.
              //2 gradient supervised targets should not overlap as it is handled when adding targets.
              //Safety HAlt in that case.
              SupervisedTarget::SupervisedTargetType newSupTargetType = supvTarg->getSupervisedTargetType();
              SupervisedTarget::SupervisedTargetType oldSupTargetType = target->getSupervisedTargetType();


              if((newSupTargetType == SupervisedTarget::GradientSupvTarget) &&
                  (oldSupTargetType == SupervisedTarget::GradientSupvTarget))
              {
                ATC::AbstractEventHandler::corePtr()->reportEvent(twoSupGradAtSamePos, __FILE__, __LINE__);
              }
              else if(newSupTargetType == SupervisedTarget::GradientSupvTarget)
              {
                target->setGradient(supvTarg->getGradient());
                targetAtSamePosFound = true;
                ret = true;
              }
              //If gradient supervised target was already there, it should be deleted and target added.
              else if(oldSupTargetType == SupervisedTarget::GradientSupvTarget)
              {
                supvTarg->setGradient(target->getGradient());
                targetAtSamePosFound = true;
                odoInsertPositionIter = it;
              }
              else
              {
                //Add both targets.
                odoInsertPositionIter = it;
                //set the gradient the same as the existing target if that target has valid gradient.
                if(target->isGradientValid())
                {
                  supvTarg->setGradient(target->getGradient());
                }
              }
            }
            else
            {
              //Do nothing
            }
          }
        }

        if ((odoInsertPositionIter != supvTargetList.end()) || targetAtSamePosFound || errorOccured)
        {
          break;
        }
      }

      if (odoInsertPositionIter != supvTargetList.end())
      {
        if(targetAtSamePosFound)
        {
          delete *odoInsertPositionIter;
          odoInsertPositionIter = supvTargetList.erase(odoInsertPositionIter);
        }
        static_cast<void>(supvTargetList.insert(odoInsertPositionIter, supvTarg));
        ret = true;
      }
      else if ((!targetAtSamePosFound) && (!errorOccured))
      { 
        //if iterator is at end of list
        supvTargetList.pushBack(supvTarg);
        ret = true;
      }
      else
      {
        //Do nothing
      }

      return ret;
    }

    /******************************************************************************
    * insertMaTargetInListInternal
    ******************************************************************************/
    bool AbstractTargets::insertMaTargetInListInternal(BaseTarget * const base)
    {
      bool ret = false;
      bool errorOccured = false;

      MaTargetIterator odoInsertPositionIter = maTargetList.end();
      MaTargetIterator it = maTargetList.begin();

      for (; it != maTargetList.end(); ++it)
      {
        const BaseTarget* const target = *it;

        if (target->getTargetId() == base->getTargetId())
        {
          //Safety halt
          //prepare the dynamic text to be send while reporting event.
          sameTargetIdError.setDynamicText(target->getTargetId());
          ATC::AbstractEventHandler::corePtr()->reportEvent(sameTargetIdError, __FILE__
            , __LINE__);
          errorOccured = true;
        }
        else
        {
          bool isTargetPosFound = false;
          if (DirForward == supposedTravelDir)
          {
            isTargetPosFound = (target->getOdometer() >= base->getOdometer());
          }
          else if (DirReverse == supposedTravelDir)
          {
            isTargetPosFound = (target->getOdometer() <= base->getOdometer());
          }
          else
          {
            //Report Error for unknown Direction while adding target
            //prepare the dynamic text to be send while reporting event.
            unknownTargetDirectionError.setDynamicText(target->getTargetId());
            ATC::AbstractEventHandler::corePtr()->reportEvent(unknownTargetDirectionError, __FILE__, __LINE__);
            errorOccured = true;
          }

          if (!errorOccured)
          {
            if (isTargetPosFound)
            {
              odoInsertPositionIter = it;
            }
            else
            {
              //Do nothing
            }
          }
        }

        if ((odoInsertPositionIter != maTargetList.end()) || errorOccured)
        {
          break;
        }
      }

      if (odoInsertPositionIter != maTargetList.end())
      {
        static_cast<void>(maTargetList.insert(odoInsertPositionIter, base));
        ret = true;
      }
      else if(!errorOccured)
      {
        maTargetList.pushBack(base);
        ret = true;
      }
      else
      {
        //Do nothing
      }

      return ret;
    }

    /******************************************************************************
    * getCurCeilingSpeed
    ******************************************************************************/
    uint32_t AbstractTargets::getCurCeilingSpeed(void) const
    {
      return curCeilingSpeed;
    }

    /******************************************************************************
    * getCurTrackGradient
    ******************************************************************************/
    int32_t AbstractTargets::getCurTrackGradient(void) const
    {
      return curTrackGradient;
    }

    /******************************************************************************
    * getCurGradient
    ******************************************************************************/
    int32_t AbstractTargets::getCurGradient(void) const
    {
      return curGradient;
    }

    /******************************************************************************
    * setCurCeilingSpeed
    ******************************************************************************/
    void AbstractTargets::setCurCeilingSpeed(const uint32_t currentCeilingSpeed)
    {
      const TrainSetup* const pTrainSetup = AbstractTSetup::corePtr()->getTrainSetup();
      if (pTrainSetup == static_cast<const TrainSetup*>(NULL))
      {
        curCeilingSpeed = currentCeilingSpeed;
      }
      else
      {
        curCeilingSpeed = ATC::ATCMath::minimum(currentCeilingSpeed, static_cast<uint32_t>(pTrainSetup->maxSpeed));
      }
    }

    /******************************************************************************
    * setCurTrackGradient
    ******************************************************************************/
    void AbstractTargets::setCurTrackGradient(const int32_t currentTrackGradient)
    {
      curTrackGradient = currentTrackGradient;
    }

    /******************************************************************************
    * setCurGradient
    ******************************************************************************/
    void AbstractTargets::setCurGradient(const int32_t currentGradient)
    {
      curGradient = currentGradient;
    }

    /******************************************************************************
    * isTargetDelInList
    ******************************************************************************/
    bool AbstractTargets::isTargetDelInList(void) const
    {
      return isTargetDeletedInList;
    }

    /******************************************************************************
    * setSMSpeedRestriction
    ******************************************************************************/
    void AbstractTargets::setSMSpeedRestriction(const uint32_t smRestriction)
    {
      smSpeedRestriction = smRestriction;
    }

    /******************************************************************************
    * getSMSpeedRestriction
    ******************************************************************************/
    uint32_t AbstractTargets::getSMSpeedRestriction(void) const
    {
      return smSpeedRestriction;
    }

    /******************************************************************************
    * setConditionalTargetSupervise
    ******************************************************************************/
    //lint -esym(1714,ATP::DS::AbstractTargets::setConditionalTargetSupervise) Will be used in other projects
    void AbstractTargets::setConditionalTargetSupervise(const uint32_t targetId, const bool supervise)
    {
      MaTargetIterator it = maTargetList.begin();
      for (it = maTargetList.begin(); (it != maTargetList.end()); ++it)
      {
        if ((*it)->getTargetId() == targetId)
        {
          if (BaseTarget::ConditionalTarget == (*it)->getTargetType())
          {
            ConditionalTarget* condTarg = ATC::dynamicCast<BaseTarget*, ConditionalTarget*>(*it, __FILE__, __LINE__);
            targetListChanged = condTarg->setSupervised(supervise);
            break;
          }
        }
      }
    }

    /******************************************************************************
    * consoleCall
    ******************************************************************************/
    bool AbstractTargets::consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv)
    {
      /*
      This functions parses the arguments searches for the "help", "trace" or any other Console
      component specific command calls and handles it. Returns true if completely handled
      else returns false. returning false will let other components handle the call. help always returns false.
      */

      bool completelyHandled = false;

      // Handle help call at first. argc cannot be 0 as there is a check before consoleCall()
      if (ATC::isTextMatch(&argv[0][0], "help", sizeof("help")) && (argc == 1U))
      {
        const char_t* const helpText =
          "targets       To list all targets including Ma and Supervised list\n"
          "maTarg        To list all targets in MA target list\n"
          "supvTarg      To list all targets in supervised target list\n"
          "curCS         To get the current ceiling speed\n"
          "curGrad       To get the current gradient used\n";

        ATC::AbstractConsole::corePtr()->write(helpText);
        completelyHandled = false;
      }
      else if (ATC::isTextMatch(&argv[0][0], "maTarg", sizeof("maTarg")) && (argc == 1U))
      {
        displayMaTarg();
        completelyHandled = true;
      }
      else if (ATC::isTextMatch(&argv[0][0], "supvTarg", sizeof("supvTarg")) && (argc == 1U))
      {
        displaySupTarg();
        completelyHandled = true;
      }
      else if (ATC::isTextMatch(&argv[0][0], "targets", sizeof("targets")) && (argc == 1U))
      {
        displayMaTarg();
        displaySupTarg();
        completelyHandled = true;
      }
      else if (ATC::isTextMatch(&argv[0][0], "curCS", sizeof("curCS")) && (argc == 1U))
      {
        char_t  buffer[512];
        const int32_t result = snprintf(&buffer[0], sizeof(buffer), "Current CS=%d, FwCS=%d, SwCS=%d, SbCS=%d, EbCs=%d cm/s",
            getCurCeilingSpeed(), getCurrFwCeilingSpeed(), getCurrSwCeilingSpeed(), getCurrSbCeilingSpeed(),
            getCurrEbCeilingSpeed());
        if ((result > 0)  &&  (static_cast<size_t>(result) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }
        completelyHandled = true;
      }
      else if (ATC::isTextMatch(&argv[0][0], "curGrad", sizeof("curGrad")) && (argc == 1U))
      {
        char_t  buffer[512];
        int32_t curTrackGrad = getCurTrackGradient();
        int32_t curGrad = getCurGradient();
        const int32_t result = snprintf(&buffer[0], sizeof(buffer),
          "Current Gradient: %d permil  Current Track Gradient(last removed gradient target): %d permil", curGrad, curTrackGrad);

        if ((result > 0)  &&  (static_cast<size_t>(result) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }
        completelyHandled = true;
      }
      else
      {
        //Do Nothing
      }

      return completelyHandled;
    }

    /******************************************************************************
    * Set Odometer Invalid Target Status
    ******************************************************************************/
    void AbstractTargets::setOdometerInvalidTargetActive(const bool status)
    {
      odometerInvalidTargetActive = status;
    }

    /******************************************************************************
    * Set Free Rolling Target Status
    ******************************************************************************/
    void AbstractTargets::setFreeRollingTargetActive(const bool status)
    {
      freeRollingTargetActive = status;
    }

    /******************************************************************************
    *Get Free Rolling Target Status
    ******************************************************************************/
    bool AbstractTargets::getFreeRollingTargetActive() const
    {
      return freeRollingTargetActive;
    }

    /******************************************************************************
    * Get Odometer Invalid Target Status
    ******************************************************************************/
    bool AbstractTargets::getOdometerInvalidTargetActive() const
    {
      return odometerInvalidTargetActive;
    }

    /******************************************************************************
    *removeTargetsAtStandStillInLocation
    ******************************************************************************/
    void AbstractTargets::removeTargetsAtStandStillInLocation()
    {
      if (ATPModeLocation == Kernel::AbstractModeControl::corePtr()->getCurrentMode())
      {
        MaTargetIterator it = maTargetList.begin();

        while (it != maTargetList.end())
        {
          const bool isPrimaryTargetFound = (BaseTarget::PrimaryTarget == (*it)->getTargetType());
          if (!isPrimaryTargetFound)
          {
            delAllRelatedSupvTarget(*it);
            //Gradient and ceiling speed will be updated in mode control
            delete(*it);
            it = maTargetList.erase(it);
            isTargetDeletedInList = true;
          }
          else
          {
            ++it;
          }
        }
      }
    }

    /******************************************************************************
    * removeNormalPrimaryTargetInLocation
    ******************************************************************************/
    void AbstractTargets::removeNormalPrimaryTargetInLocation()
    {
      MaTargetIterator targetIt = maTargetList.begin();
      while (targetIt != maTargetList.end())
      {
        const BaseTarget* const target = *targetIt;
        const uint8_t routeType = target->getRouteType();
        const bool islocationType = ((routeType == static_cast<uint8_t>(BaseTarget::LocationStartTargetType)) || 
                                     (routeType == static_cast<uint8_t>(BaseTarget::LocationEndTargetType)));

        if ((target->getTargetType() == BaseTarget::PrimaryTarget)  && (!islocationType))
        {
          delAllRelatedSupvTarget(target);
          delete target;
          targetIt = maTargetList.erase(targetIt);
          isTargetDeletedInList = true;
          
          // Always consider primary target to be reached (for evaluation of MA Timeout) if dropping out of Location
          // because of Stop Train or Emergency Alert
          reachedPrimaryTarget = true;

          //set target list changed to trigger the target calculation.
          targetListChanged = true;
        }
        else
        {
          ++targetIt;
        }
      }

    }

    /******************************************************************************
    * removeSMSpeedRestrictionTarget
    ******************************************************************************/
    void AbstractTargets::removeSMSpeedRestrictionTarget()
    {
      SupervisedTargetIterator it = supvTargetList.begin();
      while (it != supvTargetList.end())
      {
        if ((*it)->getSupervisedTargetType() == SupervisedTarget::SMSpeedRestrictionTarget)
        {
          //reset the CS restriction when deleting the target
          setSMSpeedRestriction(0U);
          delete(*it);
          it = supvTargetList.erase(it);
        }
        else
        {
          ++it;
        }
      }
    }

    /******************************************************************************
    * Delete the all the target at standstill
    ******************************************************************************/
    void AbstractTargets::setDelTargetAtStandStill(const bool status)
    {
      delTargetAtStandStill = status;
    }

    /******************************************************************************
    * Manage Emergency Stop Active Signal Input
    ******************************************************************************/
    void AbstractTargets::manageEMSInput(void)
    {
      // Check if emergency stop active signal is active
      const bool emergencyStopActive = IO::AbstractLocoIO::corePtr()->getEmergencyStopActiveAlert();

      //Check if emergency brake applied
      const bool isEBApplied = Supv::AbstractBrake::corePtr()->getEbApplied();

      if (Pos::AbstractOdometry::corePtr()->isTrainStandStill() && emergencyStopActive && (!isEBApplied))
      {
        //AOS deletes all targets if Vehical is at stand-still, EMS is active and EB is not applied
        removeAll();
      }
    }

    /******************************************************************************
    * getSafetyMargin
    ******************************************************************************/
    uint16_t AbstractTargets::getSafetyMargin() const
    {
      return currentSafetyMargin;
    }

    /******************************************************************************
    * getWriteCrossCompareMaxSize
    ******************************************************************************/
    //lint -esym(1714,ATP::DS::AbstractTargets::getWriteCrossCompareMaxSize) Lint is wrong, this *is* used
    uint32_t AbstractTargets::getWriteCrossCompareMaxSize() const
    {
      uint32_t ccMaxSize = 0U;

      {
        ConstMaTargetIterator iter = maTargetList.begin();
        const ConstMaTargetIterator iterEnd = maTargetList.end();

        while (iter != iterEnd)
        {
          ccMaxSize += (*iter)->getWriteCrossCompareMaxSize();
          ++iter;
        }
      }

      {
        ConstSupervisedTargetIterator iter = supvTargetList.begin();
        const ConstSupervisedTargetIterator iterEnd = supvTargetList.end();

        while (iter != iterEnd)
        {
          ccMaxSize += (*iter)->getWriteCrossCompareMaxSize();
          ++iter;
        }
      }

      return ccMaxSize;
    }


    /******************************************************************************
    * writeCrossCompare
    ******************************************************************************/
    //lint -esym(1714,ATP::DS::AbstractTargets::writeCrossCompare) Lint is wrong, this *is* used
    void AbstractTargets::writeCrossCompare(VFW_Buffer * const buffer) const
    {
      {
        ConstMaTargetIterator iter = maTargetList.begin();
        const ConstMaTargetIterator iterEnd = maTargetList.end();

        while (iter != iterEnd)
        {
          (*iter)->writeCrossCompare(buffer);
          ++iter;
        }
      }

      {
        ConstSupervisedTargetIterator iter = supvTargetList.begin();
        const ConstSupervisedTargetIterator iterEnd = supvTargetList.end();

        while (iter != iterEnd)
        {
          (*iter)->writeCrossCompare(buffer);
          ++iter;
        }
      }
    }

    /******************************************************************************
    * setSafetyMarginChangeTargetActive
    ******************************************************************************/
    void AbstractTargets::setSafetyMarginChangeTargetActive(const uint16_t value)
    {
      writeToLog(ATC::BriefLog, "setSafetyMarginChangeTargetActive:", static_cast<uint32_t>(value), __FILE__, __LINE__);

      currentSafetyMargin = value;
    }

    /******************************************************************************
    * getGradientTargetList
    ******************************************************************************/
    const uint8_t AbstractTargets::getGradientTargetList(GradientTarget* gradTargArrPtr[]) const
    {
      uint8_t gradTargIndex = 0U;
      for (ConstMaTargetIterator it = maTargetList.begin();
        it != maTargetList.end(); ++it)
      {
        // If the travel direction matches target direction
        if (supposedTravelDir == (*it)->getDirection())
        {
          BaseTarget::CoreTargetType targetType = (*it)->getTargetType();
          if (BaseTarget::GradientTarget == targetType)
          {
            GradientTarget *gradTarget = ATC::dynamicCast<BaseTarget*, GradientTarget*>(*it, __FILE__, __LINE__);
            if (gradTargIndex < maxNumberOfGradientTargets)
            {
              gradTargArrPtr[gradTargIndex] = gradTarget;
              ++gradTargIndex;
            }
            else
            {
              ATC::AbstractEventHandler::corePtr()->reportEvent(contSizeExceeded, __FILE__, __LINE__);
            }
          }
        }
      }
      return gradTargIndex;
    }


    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void AbstractTargets::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization
      Support::AbstractCrossCompare* const crossCompare = Support::AbstractCrossCompare::corePtr();

      // Add all events...
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&invalidGradientMemAllocation));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&invalidPrimaryMemAllocation));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&invalidSpeedMemAllocation));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&invalidSupervisedMemAllocation));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&invalidTrackDataMemAllocation));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&invalidKeepTrackDataMemAllocation));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&invalidDirection));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&targetListFullError));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&initNotDone));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&baseTargetNULLError));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&unknownTargetDirectionError));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&inconsistencyTargetTrack));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&sameTargetIdError));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&contSizeExceeded));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <ATC::Event>(&twoSupGradAtSamePos));
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&nullPointerAcess));


      // Other attributes...
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&odometerInvalidTargetActive));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&freeRollingTargetActive));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&coreInitDone));

      crossCompare->addCrossCompareData(new Support::CrossCompareEnum<TravelDir>(&supposedTravelDir));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&targetListChanged));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&targetListReversed));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&curCeilingSpeed));
      crossCompare->addCrossCompareData(new Support::CrossCompareInt32(&curTrackGradient));
      crossCompare->addCrossCompareData(new Support::CrossCompareInt32(&curGradient));

      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&curFWCeilingSpeed));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&curSWCeilingSpeed));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&curSBCeilingSpeed));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint32(&curEBCeilingSpeed));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint8(&adhesionValue));

      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&delTargetAtStandStill));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint16(&currentSafetyMargin));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&isTargetDeletedInList));

      // For the complex data, i.e. target list object, handle this in writeCrossCompare()
      crossCompare->addCrossCompareData(new Support::CrossCompareComplex <AbstractTargets>(this));
    }

    /******************************************************************************
    * getCurrFwCeilingSpeed
    ******************************************************************************/
    uint32_t AbstractTargets::getCurrFwCeilingSpeed(void) const
    {
      return curFWCeilingSpeed;
    }

    /******************************************************************************
    * getCurrSwCeilingSpeed
    ******************************************************************************/
    uint32_t AbstractTargets::getCurrSwCeilingSpeed(void) const
    {
      return curSWCeilingSpeed;
    }

    /******************************************************************************
    * getCurrSwCeilingSpeed
    ******************************************************************************/
    uint32_t AbstractTargets::getCurrSbCeilingSpeed(void) const
    {
      return curSBCeilingSpeed;
    }

    /******************************************************************************
    * getCurrSwCeilingSpeed
    ******************************************************************************/
    uint32_t AbstractTargets::getCurrEbCeilingSpeed(void) const
    {
      return curEBCeilingSpeed;
    }

    /******************************************************************************
    * setCurrFwCeilingSpeed
    ******************************************************************************/
    void AbstractTargets::setCurrFwCeilingSpeed(const uint32_t fwCS)
    {
      curFWCeilingSpeed = fwCS;
    }

    /******************************************************************************
    * setCurrSwCeilingSpeed
    ******************************************************************************/
    void AbstractTargets::setCurrSwCeilingSpeed(const uint32_t swCS)
    {
      curSWCeilingSpeed = swCS;
    }

    /******************************************************************************
    * setCurrSbCeilingSpeed
    ******************************************************************************/
    void AbstractTargets::setCurrSbCeilingSpeed(const uint32_t sbCS)
    {
      curSBCeilingSpeed = sbCS;
    }

    /******************************************************************************
    * setCurrEbCeilingSpeed
    ******************************************************************************/
    void AbstractTargets::setCurrEbCeilingSpeed(const uint32_t ebCS)
    {
      curEBCeilingSpeed = ebCS;
    }

    /******************************************************************************
    * displayMaTarg
    ******************************************************************************/
    void AbstractTargets::displayMaTarg()
    {
      char_t  buffer[512];
      if (!maTargetList.empty())
      {
        int32_t result = snprintf(&buffer[0], sizeof(buffer), "%-8s%-8s%-8s%-8s%-8s%-8s",
          "Type", "Trk", "Pos", "Odo", "ID", "Dir");

        if ((result > 0)  &&  (static_cast<size_t>(result) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }

        for (MaTargetIterator targetItr = maTargetList.begin(); targetItr != maTargetList.end(); ++targetItr)
        {
          BaseTarget* trg = (*targetItr);
          result = snprintf(&buffer[0], sizeof(buffer), "%-8u%-8u%-8u%-8d%-8u%-8u",
            trg->getTargetType(), trg->getPosition().track, trg->getPosition().position,
            trg->getOdometer(), trg->getTargetId(), trg->getDirection());
          if ((result > 0)  &&  (static_cast<size_t>(result) < sizeof(buffer)))
          {
            ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
          }
        }
      }
      else
      {
        ATC::AbstractConsole::corePtr()->writeWithNewline("No Targets in MA Target List");
      }
    }

    /******************************************************************************
    * displaySupTarg
    ******************************************************************************/
    void AbstractTargets::displaySupTarg()
    {
      char_t  buffer[512];
      if (!supvTargetList.empty())
      {
        int32_t result = snprintf(&buffer[0], sizeof(buffer), "%-8s%-8s%-8s%-8s%-8s%-8s%-8s%-8s%-8s%-8s%-8s%-8s%-8s%-8s%-8s%-8s%-8s",
          "Type", "Trk", "Pos", "Odo", "ID", "Dir", "parntId", "Grad", "CS", "FwCs",
          "FwSpeed" ,"SwCs", "SwSpeed","SbCs", "SbSpeed","EbCs", "EbSpeed");

        if ((result > 0)  &&  (static_cast<size_t>(result) < sizeof(buffer)))
        {
          ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
        }

        for (SupervisedTargetIterator targetItr = supvTargetList.begin(); targetItr != supvTargetList.end(); ++targetItr)
        {
          SupervisedTarget* trg = *targetItr;
          result = snprintf(&buffer[0], sizeof(buffer), "%-8u%-8u%-8u%-8d%-8u%-8u%-8u%-8d%-8u%-8u%-8u%-8u%-8u%-8u%-8u%-8u%-8u",
            trg->getSupervisedTargetType(), trg->getPosition().track, trg->getPosition().position,
            trg->getOdometer(),trg->getTargetId(), trg->getDirection(), trg->getParentTarget()->getTargetId(),
            trg->getGradient(), trg->getCeilingSpeed(), trg->getFwCeilingSpeed(), trg->getFirstWarningSpeed(),
            trg->getSwCeilingSpeed(), trg->getSecondWarningSpeed(), trg->getSbCeilingSpeed(), trg->getSBSpeed(),
            trg->getEbCeilingSpeed(), trg->getEBSpeed());

          if ((result > 0)  &&  (static_cast<size_t>(result) < sizeof(buffer)))
          {
            ATC::AbstractConsole::corePtr()->writeWithNewline(&buffer[0]);
          }
        }
      }
      else
      {
        ATC::AbstractConsole::corePtr()->writeWithNewline("No Targets in Supervised Target List");
      }
    }

    /******************************************************************************
    * RadioMessageInConfigurationData::veryDetailedLog
    ******************************************************************************/
    void AbstractTargets::veryDetailedLog(void) const
    {
      uint8_t currentLevel;
      bool isEnabled;

      trace.getTraceDetails(currentLevel, isEnabled);

      if (isEnabled && (currentLevel >= ATC::veryDetailedTrace))
      {   // No reason to assemble logStr if trace not enabled
        char_t logStr[120];

        if (oldFreeBlocksPrim != freeBlocksPrim)
        {
          const int32_t result = snprintf(&logStr[0], sizeof(logStr), "Free Primary Target Blocks = %u", freeBlocksPrim);

          if ((result > 0)  &&  (static_cast<size_t>(result) < sizeof(logStr)))
          {
            traceLog(ATC::veryDetailedTrace, ATC::DetailedLog, &logStr[0]);
          }
        }

        if (oldFreeBlocksGrad != freeBlocksGrad)
        {
          const int32_t result = snprintf(&logStr[0], sizeof(logStr), "Free Gradient Target Blocks = %u", freeBlocksGrad);
          if ((result > 0)  &&  (static_cast<size_t>(result) < sizeof(logStr)))
          {
            traceLog(ATC::veryDetailedTrace, ATC::DetailedLog, &logStr[0]);
          }
        }

        if (oldFreeBlocksSpeed != freeBlocksSpeed)
        {
          const int32_t result = snprintf(&logStr[0], sizeof(logStr), "Free Speed Target Blocks = %u", freeBlocksSpeed);
          if ((result > 0)  &&  (static_cast<size_t>(result) < sizeof(logStr)))
          {
            traceLog(ATC::veryDetailedTrace, ATC::DetailedLog, &logStr[0]);
          }
        }

        if (oldFreeBlocksTDI != freeBlocksTDI)
        {
          const int32_t result = snprintf(&logStr[0], sizeof(logStr), "Free TrackDataItem Target Blocks = %u", oldFreeBlocksTDI);
          if ((result > 0)  &&  (static_cast<size_t>(result) < sizeof(logStr)))
          {
            traceLog(ATC::veryDetailedTrace, ATC::DetailedLog, &logStr[0]);
          }
        }

        if (oldFreeBlocksSupervised != freeBlocksSupervised)
        {
          const int32_t result = snprintf(&logStr[0], sizeof(logStr), "Free Supervised Target Blocks = %u", freeBlocksSupervised);
          if ((result > 0)  &&  (static_cast<size_t>(result) < sizeof(logStr)))
          {
            traceLog(ATC::veryDetailedTrace, ATC::DetailedLog, &logStr[0]);
          }
        }
      }
    }

    /******************************************************************************
    * traceLog
    ******************************************************************************/
    void AbstractTargets::traceLog(uint8_t const traceLevel, ATC::LogLevel const logLevel, const char_t* const text) const
    {
      trace.write(traceLevel, text);
      writeToLog(logLevel, text);
    }
  }
}

//lint +esym(586,snprintf)
