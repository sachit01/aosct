#ifndef LCSMessageOutTrainComposition_hpp
#define LCSMessageOutTrainComposition_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
* DESCRIPTION: 
*  The creators for outgoing messages are inherited from AbstractLCSMessageOut.
*  One creator per message-type.
*  The LCSMessageOutTrainComposition creator is responsible for collecting 
*  final train composition data
*  from other components and validation and creation of the outgoing data in network order.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-05-10    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_types.hpp"
#include "abstract_lcs_message_out.hpp"
#include "emp_message.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace TG
  {

    /**
    * TrainComposition
    */
    struct TrainCompositionType
    {
      uint16_t              numberOfVehicles;
      RollingStockPositionType  rollingStockPosition[maxVehicleCount];
    };

    /**
    * LCSMessageOutTrainComposition is a creator for the outgoing LCSMessageOutTrainComposition message 
    */
    class LCSMessageOutTrainComposition : public AbstractLCSMessageOut
    {
    public:

      /**
      * Constructor for the creator of the outgoing LCSMessageOutTrainComposition message
      */
      LCSMessageOutTrainComposition();

      /**
      * Validates the collected input data and creates the outgoing message in network byte-order
      * 
      * @param[in]  mData   Buffer to be used for validated message in network order
      * @param[out] length  The length of the created output data
      *
      * @return true if data is valid and resulted in a valid outgoing message
      */
      virtual bool validate(EmpMsg* const mData, uint16_t& length);

      /**
      * Invalidates the outgoing message (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

      /**
      * Collects the messageType- and mode-dependent data from other components
      */
      virtual void collectData();


    private:

      /**
      * Assembles the collected data
      *
      * @param[in] messageData   The incoming message data to be assembled
      * @param[out] appDataLength  The length of the created output data
      *
      * @return true if data is valid with respect to assembling
      */
      bool assembleMessageData(EmpMsg* const messageData, uint16_t& appDataLength) const;

     /**
      * The collected data used to create the outgoing message
      * Will be cleared each ATP execution-cycle by invalidate()
      */
      TrainCompositionType trainComposition;
    };
  }
}
#endif
