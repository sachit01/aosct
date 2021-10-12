#ifndef LCSMessageOutATPCommand_hpp
#define LCSMessageOutATPCommand_hpp
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
*  The LCSMessageOutATPCommand creator is responsible for collecting the Total Train Weight,
*  the request status of ECPB Train Composition, Holding Brake and Yard Mode
*  from other components and validation and creation of the outgoing data in network order.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-05-25    nsyed       Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_timer.h>

#include "atc_types.hpp"
#include "abstract_lcs_message_out.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace TG
  {
    /**
    * ECPB Train Composition Request Status
    */
    enum EcpbTrainCompReqStatus
    {      
      EcpbTrainCompNotRequested = 0,
      EcpbTrainCompRequested = 1,
      EcpbTrainCompNotAsserted = 255
    };

    /**
    * Holding Brake Request Status
    */
    enum HoldingBrakeReqStatus
    {
      HoldingBrakeNotRequested = 0,
      HoldingBrakeRequested = 1,
      HoldingBrakeNotAsserted = 255
    };

    /**
    * Structure for ATP Command
    */
    struct LCSATPCommandType
    {
      EcpbTrainCompReqStatus  ecpbTrainCompReqStatus;
      HoldingBrakeReqStatus   holdingBrakeReqStatus;
      uint32_t                totalTrainWeight;  // In Mg (1000 000g)
    };

    /**
    * LCSMessageOutATPCommand is a creator for the outgoing ATP Command Message
    */
    class LCSMessageOutATPCommand : public AbstractLCSMessageOut
    {
    public:

      /**
      * Constructor for the creator of the outgoing ATP Command message
      */
      LCSMessageOutATPCommand();

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

      /** The time when the last Composition Request was sent (reference time, ms)
      */
      int64_t compositionRequestSentTime;

      /** Timeout time for send Composition Request (ms)
      */
      static const int64_t timeoutSendCompositionRequest = 2000;

      /**
      * The collected data used to create the outgoing message
      * Will be cleared each ATP execution-cycle by invalidate()
      */
      LCSATPCommandType atpCommand;
    };
  }
}
#endif
