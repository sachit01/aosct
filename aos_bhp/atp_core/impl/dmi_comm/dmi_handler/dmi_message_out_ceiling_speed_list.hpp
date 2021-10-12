#ifndef DMIMessageOutCeilingSpeedList_hpp
#define DMIMessageOutCeilingSpeedList_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
*  The creators for outgoing messages are inherited from AbstractDMIMessageOut.
*  One creator per message-type.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-09-21    akushwah    Created
* 2016-10-06    akushwah    Initial Implementation
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_dmi_handler.hpp"
#include "abstract_dmi_message_out.hpp"
#include "atp_types.hpp"
/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace DMICom
  {
    /**
    * Maximum number of the Speed Data Blocks
    * 1 additional Ceiling speed target at the beginning.
    */    static const uint16_t maxNoOfSpeedDataBlocks = maxNumberOfSpeedTargets + 1U;    //lint -esym(551,ATP::DMICom::maxNoOfSpeedDataBlocks) Lint is wrong, this constant *is* used
    /**
    * DMIMessageOutCeilingSpeedList is a creator for the outgoing Ceiling Speed List DMIMessage.
    */
    class DMIMessageOutCeilingSpeedList : public AbstractDMIMessageOut
    {

    public:
      /**
      * Constructor for a creator of a outgoing Ceiling Speed List DMIMessage.
      */
      DMIMessageOutCeilingSpeedList();

      /**
      * Validates the collected input data and creates the outgoing message in network byte-order
      *
      * @return true if data is valid and resulted in a valid outgoing message
      */
      virtual bool validate();

      /**
      * Invalidates the outgoing message and clears the outgoing Ceiling Speed List (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

      /**
      * Collects the messageType- and mode-dependent data from other components
      */
      virtual void collectData();

    protected:

      /**
      * Assemble the collected data
      *
      * @return true if data is valid with respect to parsing
      */
      bool assembleDMIMessageData();

      /**
      * To check if a track data item need to be included as ceiling speed list
      *
      * @param[in]      type of track data item
      * @param[out]     reference for ceiling speed change reason
      * @return         true if track data item need to be included
      */
      virtual bool includeTrackDataItemInCeilingSpeedList(const uint8_t trackDataItemType, uint8_t &speedChangeReason);

      /**
      * Number of Speed Data Blocks
      */
      uint8_t noOfSpeedDatablocks;

      /**
      * Previous travel direction (Location mode)
      * Need to send ceiling speed blocks to DMI when direction has changed in Location mode
      */
      TravelDir prevTravelDir;

      /**
      * Ceiling Speed List Data Blocks
      */
      CeilingSpeedListDataBlock speedList[maxNoOfSpeedDataBlocks];

    };
  }
}
#endif
