#ifndef DMIMessageOutRadioChannelBHP_hpp
#define DMIMessageOutRadioChannelBHP_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
*  The creators for Radio Channel outgoing messages are inherited from AbstractDMIMessageOut.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-09-21    akushwah    Created
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
    * DMIMessageOutRadioChannelBHP is a creator for the outgoing Radio Channel DMIMessage.
    */
    class DMIMessageOutRadioChannelBHP : public AbstractDMIMessageOut
    {

    public:
      /**
      * Constructor for a creator of a outgoing Radio Channel DMIMessage.
      */
      DMIMessageOutRadioChannelBHP();

      /**
      * Validates the collected input data and creates the outgoing message in network byte-order
      *
      * @return true if data is valid and resulted in a valid outgoing message
      */
      virtual bool validate();

      /**
      * Invalidates the outgoing message and clears the outgoing Train Name(shall be called once per ATP execution-cycle)
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

      /** Radio Channel name received from TCC  */
      char_t radioChannel[radioChannelNameMaxLength + 1U];
    };
  }
}
#endif