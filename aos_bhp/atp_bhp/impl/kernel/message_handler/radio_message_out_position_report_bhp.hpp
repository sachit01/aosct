#ifndef RadioMessageOutPositionReportBHP_hpp
#define RadioMessageOutPositionReportBHP_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
*  The creators for outgoing messages are inherited from AbstractRadioMessageOut.
*  One creator per message-type.
*  The RadioMessageOutPositionReport creator is responsible for collecting
*  position-report data (considering the mode etc..)
*  from other components and validation and creation of the outgoing data in network order.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-03-21    akushwah    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

#include "radio_message_out_position_report.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {

    /**
    * RadioMessageOutPositionReport is a creator for the outgoing Position Report Message
    */
    class RadioMessageOutPositionReportBHP : public RadioMessageOutPositionReport
    {

    public:

      /**
      * Constructor for RadioMessageOutPositionReportBHP which is a creator for the outgoing 
      * Position Report Message
      *
      *  @param[in] chId   The channel-ID associated with this position report.
      *
      */
      RadioMessageOutPositionReportBHP(const uint16_t chId);

      /**
      * Invalidates the extracted data (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

      /**
      * Collects the mode-dependent data from other components
      */
      virtual void collectData();

   protected:
      /**
      * Assemble the blocks related to adaptation in Position Report message
      *
      * @param[out] buffer      The buffer to be assembled
      */
      virtual void assembleAdditionalBlocks(VFW_Buffer &buffer);

    private:

      /**
      * Default constructor (disabled)
      */
      RadioMessageOutPositionReportBHP();

      /**
      * Corresponds to the field B_BHPB_TRAIN_STATUS.
      */
      uint32_t bhpbTrainStatus;

    };
  }
}
#endif
