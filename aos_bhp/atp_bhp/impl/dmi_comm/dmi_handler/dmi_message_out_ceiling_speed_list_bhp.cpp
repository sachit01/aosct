/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Each messageType (AOS->DMI) has an associated creator class inherited from AbstractDMIMessageOut.
* This file implements adaptation(for BHP) of the creator for the outgoing Ceiling Speed List DMIMessage.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2019-04-24    skothiya    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "dmi_message_out_ceiling_speed_list_bhp.hpp"
#include "abstract_dmi_message_out.hpp"
#include "abstract_dmi_handler.hpp"
#include "abstract_message_handler.hpp"
#include "targets.hpp"
#include "abstract_mode_control.hpp"
#include "atc_math.hpp"
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
  namespace DMICom
  {
    /******************************************************************************
    * DMIMessageOutCeilingSpeedListBHP constructor
    ******************************************************************************/
    DMIMessageOutCeilingSpeedListBHP::DMIMessageOutCeilingSpeedListBHP() : DMIMessageOutCeilingSpeedList()
    {

    }

    /******************************************************************************
    * includeTrackDataItemAsCeilingSpeedChange
    ******************************************************************************/
    bool DMIMessageOutCeilingSpeedListBHP::includeTrackDataItemInCeilingSpeedList(const uint8_t trackDataItemType, uint8_t &speedChangeReason)
    {
      /*Start of approach area for level crossing will be displayed on DMI planing area as an announcement icon
      * To display it internally it will be treated as ceiling speed change so a internal
      * ceiling speed change reason has been alloted to it and the ceiling speed of previous ceiling speed target will be used for it
      */
      bool ret = false;
      if (trackDataItemType == static_cast<uint8_t>(Kernel::TrackDataTypeLevelCrossing) )
      {
        ret = true;
        speedChangeReason = DMICeilingSpeedChangeReasonLevelCrossing;
      }

      return ret;
    }
  }
}
