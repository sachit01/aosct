#ifndef DMIMessageOutGradientDataList_hpp
#define DMIMessageOutGradientDataList_hpp
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
* 2016-10-20    akushwah    Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atp_types.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace DMICom
  {
    /**
    * Maximum number of the Gradient Data Blocks.
    * 2 additional data blocks are needed for current gradient and 0 gradient at the end of list
    */    static const uint16_t maxNoOfGradientDataBlocks = maxNumberOfGradientTargets + 2U;    //lint -esym(551,ATP::DMICom::maxNoOfGradientDataBlocks) Lint is wrong, this constant *is* used

    /**
    * DMIMessageOutGradientDataList is a creator for the outgoing Gradient Data List DMIMessage
    */
    class DMIMessageOutGradientDataList : public AbstractDMIMessageOut
    {

    public:
      /**
      * Constructor for a creator of a Gradient Data List
      */
      DMIMessageOutGradientDataList();

      /**
      * Validates the collected input data and creates the outgoing message in network byte-order
      *
      * @return true if data is valid and resulted in a valid outgoing message
      */
      virtual bool validate();

      /**
      * Invalidates the outgoing message and clears the outgoing Gradient Data List(shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

      /**
      * Collects the messageType- and mode-dependent data from other components
      */
      virtual void collectData();

    private:

      /**
      * Assemble the collected data
      *
      * @return true if data is valid with respect to parsing
      */
      bool assembleDMIMessageData();

      /**
      * Number of Gradient Data Blocks
      */
      uint8_t noOfGradientDatablocks;

      /**
      * Previous travel direction (Location mode)
      * Need to send gradient data blocks to DMI when direction has changed in Location mode
      */
      TravelDir prevTravelDir;


      /**
      * Gradient Data List Blocks
      */
      GradientDataListDataBlock gradientList[maxNoOfGradientDataBlocks];
    };
  }
}
#endif
