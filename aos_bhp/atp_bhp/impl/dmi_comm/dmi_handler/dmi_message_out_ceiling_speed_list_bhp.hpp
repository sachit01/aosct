#ifndef DMIMessageOutCeilingSpeedListBHP_hpp
#define DMIMessageOutCeilingSpeedListBHP_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Adaptation class of DMIMessageOutCeilingSpeedList, .
* implementing requirement specific to BHP 
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
#include "dmi_message_out_ceiling_speed_list.hpp"
/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace DMICom
  {
    /**
    * DMIMessageOutCeilingSpeedListBHP an adaptation class for DMIMessageOutCeilingSpeedList
    */
    class DMIMessageOutCeilingSpeedListBHP : public DMIMessageOutCeilingSpeedList
    {

    public:
      /**
      * Ceiling Speed Change Reason for Level Crossing
      */
      static const uint8_t DMICeilingSpeedChangeReasonLevelCrossing = 129U;
      /**
      * Constructor
      */
      DMIMessageOutCeilingSpeedListBHP();

    protected:
      /**
      * To check if a track data item need to be included as ceiling speed list
      *
      * @param[in]      type of track data item
      * @param[out]     reference for ceiling speed change reason
      * @return         true if track data item need to be included
      */
      virtual bool includeTrackDataItemInCeilingSpeedList(const uint8_t trackDataItemType, uint8_t &speedChangeReason);

    private:

    };
  }
}
#endif
