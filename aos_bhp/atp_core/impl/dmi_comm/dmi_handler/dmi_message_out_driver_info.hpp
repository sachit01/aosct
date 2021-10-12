#ifndef DMIMessageOutDriverInfo_hpp
#define DMIMessageOutDriverInfo_hpp
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
    * DMIMessageOutDriverInfo is a creator for the outgoing Driver Info DMIMessage.
    */
    class DMIMessageOutDriverInfo : public AbstractDMIMessageOut
    {

    public:
      /**
      * Constructor for a creator of a outgoing Driver Info DMIMessage.
      */
      DMIMessageOutDriverInfo();

      /**
      * Validates the collected input data and creates the outgoing message in network byte-order
      *
      * @return true if data is valid and resulted in a valid outgoing message
      */
      virtual bool validate();

      /**
      * Invalidates the outgoing message and clears the outgoing Driver Info (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

      /**
      * Collects the messageType- and mode-dependent data from other components
      */
      virtual void collectData();

      /**
      * Logs self->messageData to RU. Assumes that collectData() and validate()
      * have been successfully called.
      *
      * Only logs the message if the state information has changed or if a certain time
      * has passed since the last call. (See configuration item "RuLogDynValuePeriod")
      */
      virtual void logToRU() const;

    private:

      /**
       * Assemble the collected data
       *
       * @return true if data is valid with respect to parsing
       */
      bool assembleDMIMessageData();

      /**
      * Permitted Driving direction
      */
      TravelDir permittedDrivingDirection;

      /**
      * Permitted Speed(km/h)
      */
      uint8_t permittedSpeed;

      /**
      * Target Speed(km/h)
      */
      uint8_t targetSpeed;

      /**
      * Time to Intervention(sec)
      */
      uint8_t timetoIntervention;

      /**
      * Remaining distance to target point (m)
      */
      uint16_t remainingDistanceToTargetPoint;

      /**
      * Remaining distance to BCA (m)
      */
      uint16_t remainingDistanceToBCA;

      /**
      * Predicted distance to stand still location(m)
      */
      uint16_t predDistToStandStillLoc;

      /**
      * MA margin (cm) of the current primary target
      */
      uint32_t maMargin;

      /**
      * Current brake delay (0.1 s) for Emergency Brake
      */
      uint32_t brakeDelayEB;

      /**
      * Current brake delay (0.1 s) for Service Brake
      */
      uint32_t brakeDelaySB;

      /**
      * Current ability to brake (cm/s2)
      */
      int32_t brakeAbility;

      /**
      * Brake(Data 11 in IF Spec ATP-MMI)
      */
      uint8_t brakeRelatedData;

      /**
      * Indication related data(Data 12 in IF Spec ATP-MMI)
      */
      uint8_t indicateRelatedData;

      /**
      * Actual driving direction in Location mode(Data 13 in IF Spec ATP-MMI)
      */
      TravelDir actualDrivingDirection;

      /**
      * sendCycleDriverInfo
      */
      static const uint16_t sendCycleDriverInfo = 10U;

      /**
      * delayDriverInfo
      */
      static const uint16_t delayDriverInfo = 3U;

    };
  }
}
#endif
