#ifndef DMIMessageOutLocoVsTrainDir_hpp
#define DMIMessageOutLocoVsTrainDir_hpp
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

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace DMICom
  {
    /**
    * DMIMessageOutLocoVsTrainDir is a creator for the outgoing Loco Vs Train Dir DMIMessage
    */
    class DMIMessageOutLocoVsTrainDir : public AbstractDMIMessageOut
    {

    public:
      /**
      * Constructor for a creator of a outgoing Loco Vs Train Dir DMIMessage
      */
      DMIMessageOutLocoVsTrainDir();

      /**
      * Validates the collected input data and creates the outgoing message in network byte-order
      *
      * @return true if data is valid and resulted in a valid outgoing message
      */
      virtual bool validate();

      /**
      * Invalidates the outgoing message and clears the outgoing Loco Vs Train Dir (shall be called once per ATP execution-cycle)
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
      * Loco Vs Train Direction
      */
      LocoVsTrainDirection locoVsTrainDirOut;

    };
  }
}
#endif
