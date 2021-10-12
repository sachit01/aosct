#ifndef DMIMessageOutTrainConfigData_hpp
#define DMIMessageOutTrainConfigData_hpp
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
    * DMIMessageOutTrainConfigData is a creator for the outgoing Train Configuration Data DMIMessage.
    */
    class DMIMessageOutTrainConfigData : public AbstractDMIMessageOut
    {

    public:
      /**
      * Constructor for a creator of a outgoing Train Configuration Data DMIMessage.
      */
      DMIMessageOutTrainConfigData();

      /**
      * Validates the collected input data and creates the outgoing message in network byte-order
      *
      * @return true if data is valid and resulted in a valid outgoing message
      */
      virtual bool validate();

      /**
      * Invalidates the outgoing message and clears the outgoing Train Configuration Data(shall be called once per ATP execution-cycle)
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
      * Train name
      */
      char_t trainName[trainNameMaxLength + 1U];

      /**
      * Train Length (m)
      */
      uint16_t trainLength;

      /**
      * Bit 0 TIMS Required
      * Bit 1 Cars connected at B == 0, Cars connected at A == 1
      */
      uint8_t timsRelatedData;

      /**
      * Distance from balise to train front(cm)
      */
      uint16_t distFromBaliseToTrainFront;

      /**
      * Distance from balise to train end (cm)
      */
      uint16_t distFromBaliseToTrainEnd;

    };
  }
}
#endif
