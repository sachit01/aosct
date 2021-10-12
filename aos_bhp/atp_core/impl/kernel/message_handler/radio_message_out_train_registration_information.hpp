#ifndef RadioMessageOutTrainRegistrationInformation_hpp
#define RadioMessageOutTrainRegistrationInformation_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
*  The creators for outgoing messages are inherited from AbstractRadioMessageOut.
*  One creator per message-type.
*  The RadioMessageOutTrainRegistrationInformation creator is responsible for collecting
*  train-registration data (considering the mode etc..)
*  from other components and validation and creation of the outgoing data in network order.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-26    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_radio_message_out.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Kernel
  {
    /**
    * Defines the storage of the collected information for TrainRegistrationInformation
    */
    struct TrainRegistrationInformation
    {
      uint16_t  baliseId;  //!< Balise identification
      uint8_t   direction; //!< Direction of train in Track
    };

    /**
    * RadioMessageOutTrainRegistrationInformation is a creator for the outgoing TrainRegistrationInformation message
    */
    class RadioMessageOutTrainRegistrationInformation : public AbstractRadioMessageOut
    {
    public:

      /**
      * Constructor for the creator of the outgoing TrainRegistrationInformation message
      */
      RadioMessageOutTrainRegistrationInformation();

      /**
      * Validates the collected input data and creates the outgoing message in network byte-order
      *
      * @return true if data is valid and resulted in a valid outgoing message
      */
      virtual bool validate();

      /**
      * Invalidates the outgoing message (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

      /**
      * Collects the messageType- and mode-dependent data from other components
      */
      virtual void collectData();

    protected:

      /**
      * No Balise Info Available
      */
      const ATC::Event noBaliseInfoAvailable;

    private:

      /**
      * Assemble the collected data
      *
      * @return true if data is valid with respect to parsing
      */
      bool assembleMessageData();

      /**
       * The collected data used to create the outgoing message
       * Will be cleared each ATP execution-cycle by invalidate()
       */
      TrainRegistrationInformation trainRegistrationInformation;

      /**
      * Boolean for check whether we found track orientation in target list
      * 
      */
      bool  orientationDataFetchedFlag;
      
    };
  }
}
#endif
