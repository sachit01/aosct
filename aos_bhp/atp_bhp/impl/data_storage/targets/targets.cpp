/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of the Targets adaptation class
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-16    lantback    Created
* 2016-04-19    lantback    Use ATC::ProcComponent, init to return bool
* 2016-07-27    akushwah    Initial Implementation
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "targets.hpp"
#include "abstract_cross_compare.hpp"

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
    Targets::Targets(void) :AbstractTargets()
    {
      initDone = false;
      approachingLevelCrossing = false;
      approachingSpeedLevelCrossing = 0U;
    }

    /******************************************************************************
    * instance
    *
    * Add additional functional description here if needed.
    * (This info is not included in doxygen documentation but may be usefull)
    *
    ******************************************************************************/
    Targets& Targets::instance(void)
    {
      static Targets theOnlyTargetsInstance;

      return theOnlyTargetsInstance;
    }

    /******************************************************************************
    * init
    ******************************************************************************/
    bool Targets::init(void)
    {
      bool coreInitValue = AbstractTargets::init();

      if ((!initDone) && coreInitValue)
      {
        //Initialize the memory for the TrackDataItemBHP Target
        initDone = TrackDataItemTargetBHP::initMemPoolSizeBHP(maxNumberOfTrackDataItemTargetBHP);

        // Cross compare
        initCrossCompare();
      }
      approachingLevelCrossing = false;
      return initDone;
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void Targets::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare* const crossCompare = Support::AbstractCrossCompare::corePtr();

      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&initDone));
      crossCompare->addCrossCompareData(new Support::CrossCompareBool(&approachingLevelCrossing));
      crossCompare->addCrossCompareData(new Support::CrossCompareUint16(&approachingSpeedLevelCrossing));

      AbstractTargets::initCrossCompare();
    }

    /******************************************************************************
    * addTargetBHP for TrackDataItemTargetBHP
    ******************************************************************************/
    void Targets::addTargetBHP(TrackDataItemTargetBHP const &t)
    {
      TrackDataItemTarget *tTarget = new TrackDataItemTargetBHP(t);
      insertMaTargetInList(tTarget);
    }

    /******************************************************************************
    * Set approaching level crossing
    ******************************************************************************/
    void Targets::setApproachingLevelCrossing(const bool status)
    {
      approachingLevelCrossing = status;
    }

    /******************************************************************************
    * get approaching level crossing status and speed
    ******************************************************************************/
    bool Targets::getApproachingLevelCrossing(uint16_t & approachSpeed) const
    {
      approachSpeed = approachingSpeedLevelCrossing;
      return approachingLevelCrossing;
    }

    /******************************************************************************
    * set approaching level crossing speed
    ******************************************************************************/
    void Targets::setApproachingSpeedLevelCrossing(const uint16_t speed)
    {
      approachingSpeedLevelCrossing = speed;
    }

    /******************************************************************************
    * removeAll
    ******************************************************************************/
    void Targets::removeAll()
    {
      AbstractTargets::removeAll();
      setApproachingLevelCrossing(false);
    }
  }
}
