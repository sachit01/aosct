/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Implementation of ConditionalTarget
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
*                           Changed setSupervised
* 2016-10-26    rquensel    Removed lint warnings
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "conditional_target.hpp"
#include "abstract_config.hpp"
#include "abstract_targets.hpp"
#include "abstract_tracks.hpp"

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
    //lint -esym(1714,ATP::DS::ConditionalTarget::ConditionalTarget) Will be used in other projects
    ConditionalTarget::ConditionalTarget(const bool tSup, const uint32_t tSpeed, 
    const TrackAndPos& tPos, const TravelDir tDir, const OdoPosition targOdo)
    : BaseTarget(BaseTarget::ConditionalTarget, tPos, tDir, targOdo)
    {
      supervised = tSup;
      speed      = tSpeed;
      statusChanged = false;
    }

    /******************************************************************************
    * Destructor
    ******************************************************************************/
    ConditionalTarget::~ConditionalTarget()
    {
    }

    /******************************************************************************
    * getSupervised
    ******************************************************************************/
    //lint -esym(1716,ATP::DS::ConditionalTarget::getSupervisedStatus) Will be used in other projects
    bool ConditionalTarget::getSupervisedStatus() const
    {
      return supervised; 
    }

    /******************************************************************************
    * getSpeed
    ******************************************************************************/
    uint32_t ConditionalTarget::getSpeedChange() const
    {
      return speed; 
    }

    /******************************************************************************
    * setSupervised
    * This function shall only be used by AbstractTargets
    * Other components need to call setConditionalTargetSupervise() from AbstractTargets
    * to set or reset the supervise status
    ******************************************************************************/
    bool ConditionalTarget::setSupervised(const bool supervise)
    {
      if (supervised != supervise) //lint !e731 Comparing boolean variables is ok
      {
        supervised = supervise;
        statusChanged = true;

        if(supervised)
        {
          addRelatedSuperviseTargets();
        }
        else
        {
          AbstractTargets::corePtr()->delAllRelatedSupvTarget(this);
        }

      }
      return statusChanged;
    }

    /******************************************************************************
    * isStatusChanged
    ******************************************************************************/
    //lint -esym(1714,ATP::DS::ConditionalTarget::isStatusChanged) Will be used in other projects
    bool ConditionalTarget::isStatusChanged(void) const
    {
      return statusChanged;
    }

    /******************************************************************************
    * resetStatusChanged
    ******************************************************************************/
    void ConditionalTarget::resetStatusChanged(void)
    {
      statusChanged = false;
    }

    /******************************************************************************
    * addRelatedSuperviseTargets
    ******************************************************************************/
    void ConditionalTarget::addRelatedSuperviseTargets(void)
    {
      const TravelDir tDir = getDirection();
      const OdoPosition targOdo = getOdometer();

      int32_t ebOdometer = targOdo;
      int32_t sbOdometer = targOdo;

      TrackAndPos ebTnP = AbstractTracks::corePtr()->calculateTrackAndPos(ebOdometer);
      TrackAndPos sbTnP = AbstractTracks::corePtr()->calculateTrackAndPos(sbOdometer);

      DS::SupervisedTarget ebTarget(SupervisedTarget::EBPrimaryTarget, this, ebTnP, tDir, ebOdometer);
      AbstractTargets::corePtr()->addTarget(ebTarget);

      DS::SupervisedTarget sbTarget(SupervisedTarget::SBPrimaryTarget, this, sbTnP, tDir, sbOdometer);
      AbstractTargets::corePtr()->addTarget(sbTarget);
    }
 }
}
