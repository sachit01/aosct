#ifndef DMIMessageOutAtpNotification_hpp
#define DMIMessageOutAtpNotification_hpp
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
* 2016-10-26    marlundg    Created
*
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
    * DMIMessageOutAtpNotification is a creator for the outgoing AtpNotification DMIMessage
    */
    class DMIMessageOutAtpNotification : public AbstractDMIMessageOut
    {

    public:
      /**
      * Constructor for a creator of an AtpNotification message
      */
      DMIMessageOutAtpNotification();

      /**
      * Validates the collected input data and creates the outgoing message in network byte-order
      *
      * @return true if data is valid and resulted in a valid outgoing message
      */
      virtual bool validate();

      /**
      * Invalidates the outgoing message and clears the data (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

      /**
      * Collects the data from other components
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
      * Create the ATP Notification text that is shown on DMI
      *
      * @param[in] info                The given reason info
      * @param[in] unknownReasonValue  The value for unknown reason
      */
      void createATPNoficiationText(const uint16_t info, const uint16_t unknownReasonValue);

      /**
      * Max number of chars in text
      */
      static const uint8_t textSize = 100U;

      /**
      * Text to be translated by DMI
      */
      char_t text[textSize];

      /**
      * Scaler for the Unregistration reasons
      * Same notification message is used for the Reject Configuration and Unregistration reasons
      * Therefore to identify the reasons on DMI software Unregistration reasons numbering start from 10.
      */
      static const uint8_t dmiScalerForUnregReason = 10U;
    };
  }
}
#endif
