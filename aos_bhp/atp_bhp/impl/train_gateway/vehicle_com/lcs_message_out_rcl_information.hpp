#ifndef LCSMessageOutRclInformation_hpp
#define LCSMessageOutRclInformation_hpp    
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2020
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
* DESCRIPTION: 
*  The creators for outgoing messages are inherited from AbstractLCSMessageOut.
*  One creator per message-type.
*  The LCSMessageOutRclInformation creator is responsible for collecting 
*  status data (considering the mode, position etc.)
*  from other components and validation and creation of the outgoing data in network order.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
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
    * Structure for LCS Status
    */
    struct LCSRclInformationType
    {
      /**
      * Default constructor. Initializes all members of the struct.
      */
      LCSRclInformationType();

      /** AOS Operational mode
      */
      AosOperationalModeType  aosOperationalMode;

      /** AOS intervention applied
      */
      AosInterventionType  aosInterventionApplied;

      /** Allowed train movement
      */
      AllowedTrainMovementType  allowedTrainMovement;

      /** Distance to go, forward
      */
      int32_t  distanceToGoForward;

      /** Distance to go, Reverse
      */
      int32_t  distanceToGoReverse;

      /** Train orientation
      */
      TrainOrientationType  trainOrientation;

      /** Current ceiling speed
      */
      uint16_t currentCeilingSpeed;
    };

    /**
    * LCSMessageOutRclInformation is a creator for the outgoing Status message 
    */
    class LCSMessageOutRclInformation : public AbstractLCSMessageOut
    {
    public:

      /**
      * Constructor for the creator of the outgoing Rcl Information message
      */
      LCSMessageOutRclInformation();

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
      LCSRclInformationType rclInfo;

      /** Counter for the timing of sending the message
      */
      uint32_t sendRclInfoMessageCounter;

      /** Period time for sending the message (ms)
      */
      static const uint32_t timeoutSendRclInfoMessage = 500U;


      /** Train Orientation, when cars are connected to A end of locomotive.
      */
      static const uint32_t trainOrientationCarConnectedToAEnd = 4U;
    };
  }
}
#endif
