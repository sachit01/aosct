#ifndef LCSMessageOutStatus_hpp
#define LCSMessageOutStatus_hpp
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
*  The LCSMessageOutStatus creator is responsible for collecting 
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
* 2016-11-23    marlundg    Created
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
    struct LCSAOSStatusType
    {
      /**
      * Default constructor. Initializes all members of the struct.
      */
      LCSAOSStatusType();

      ATOModeCabinSelectorType   atoModeCabinSelectorStatus;
      TrainIdlingType   trainIdling;
      
      uint16_t  trackIdFrontOfTrain;
      uint32_t  positionOnTrackFrontOfTrain;
      TrainOrientationFrontTrackType  trainOrientationFrontTrack;
      
      uint16_t  trackIdRearOfTrain;
      uint32_t  positionOnTrackRearOfTrain;
      TrainOrientationRearTrackType   trainOrientationRearTrack;

      TravelDirectionType   travelDirection;

      uint16_t vehicleSpeed;
      BlueFlagStatusType  blueFlagStatus;
      uint32_t  messageTime;
      LimitedSupervisedModeType limitedSupervisedMode;
    };

    /**
    * LCSMessageOutStatus is a creator for the outgoing Status message 
    */
    class LCSMessageOutStatus : public AbstractLCSMessageOut
    {
    public:

      /**
      * Constructor for the creator of the outgoing Status message
      */
      LCSMessageOutStatus();

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

      /**
      * Logs the given message to RU. Assumes that validate() has been called
      * successfully for this message.
      *
      * Only logs the message if the state information has changed or if a certain time
      * has passed since the last call. (See configuration item "RuLogDynValuePeriod")
      */
      virtual void logToRU(const EmpMsg* const mData) const;

    private:

      /**
      * Gets the cabin selector status from I/O
      *
      * @param[out] selectorType  The value of selected cabin selector
      *
      ******************************************************************************/
      void getCabinSelector(ATOModeCabinSelectorType &selectorType) const;

      /** Counter for the timing of sending the message
      */
      uint32_t sendStatusMessageCounter;

      /** Period time for sending the message (ms)
      */
      static const uint32_t timeoutSendStatusMessage = 500U;

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
      LCSAOSStatusType aosStatus;
    };
  }
}
#endif
