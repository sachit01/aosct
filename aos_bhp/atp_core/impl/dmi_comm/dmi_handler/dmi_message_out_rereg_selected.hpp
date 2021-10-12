#ifndef DMIMessageOutReRegSelected_hpp
#define DMIMessageOutReRegSelected_hpp
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
* 2017-12-11    spandita    Renamed the class name
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
    * DMIMessageOutReRegSelected is a creator for the outgoing Re-Registration Selected DMIMessage
    */
    class DMIMessageOutReRegSelected : public AbstractDMIMessageOut
    {

    public:
      /**
      * Constructor for a creator of a outgoing Re-Registration Selected DMIMessage
      */
      DMIMessageOutReRegSelected();

      /**
      * Validates the collected input data and creates the outgoing message in network byte-order
      *
      * @return true if data is valid and resulted in a valid outgoing message
      */
      virtual bool validate();

      /**
      * Invalidates the outgoing message and clears the outgoing Re-Registration Selected(shall be called once per ATP execution-cycle)
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
      * Bit 0 TIMS Required
      * Bit 1 Cars connected at B == 0, Cars connected at A == 1
      */
      uint8_t timsRelatedData;

    };
  }
}
#endif
